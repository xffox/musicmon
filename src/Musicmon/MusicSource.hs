{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Musicmon.MusicSource where

import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Exception as Exception
import qualified Text.Read as Read
import qualified Text.Printf as Printf
import qualified System.Clock as Clock
import qualified Data.Time.Clock as UTCClock
import qualified Network.MPD as MPD

import qualified Musicmon.Model as Model
import qualified Musicmon.Config as Config

getState :: (MonadIO.MonadIO m, Except.MonadError String m) =>
  Maybe Config.ConfigMPD -> m (Model.PlayerState, Model.Timestamp)
getState config = do
    state <- runMPD config makePlayerState
    case state of
      Right st -> do
        ts <- makeTimestamp
        return (st, ts)
      Left err -> Except.throwError err

waitState :: (MonadIO.MonadIO m, Except.MonadError String m) =>
  Maybe Config.ConfigMPD -> m (Model.PlayerState, Model.Timestamp)
waitState config = do
  state <- runMPD config $ do
    waitChange
    makePlayerState
  case state of
    Right st -> do
      ts <- makeTimestamp
      return (st, ts)
    Left err -> Except.throwError err

makePlayerState = do
  st <- MPD.status
  case MPD.stState st of
    MPD.Stopped -> return $ Right $ Model.StateStopped
    MPD.Playing -> (fmap Model.StatePlaying) <$> (prepareSong st)
    MPD.Paused -> (fmap Model.StatePaused) <$> (prepareSong st)
  where prepareSong st = do
          maybeSong <- fmap maybeHead $ MPD.playlistId $ MPD.stSongID st
          case maybeSong of
            Just s -> return $ Right $ toModelSong s
            _ -> return $ Left "MPD error: failed to find song"

findSong songID = do
  songs <- MPD.find (MPD.qFile songID)
  case songs of
    (s:_) -> return $ Just s
    _ -> return Nothing

toModelSong song =
   Model.Song
        (Maybe.fromMaybe "" $ maybeTagText MPD.Title)
        (tagOrDefault 0 MPD.Track)
        (tagOrDefault 0 MPD.Disc)
        (Maybe.fromMaybe "" $ maybeTagText MPD.Album)
        (Maybe.fromMaybe "" $ maybeTagText MPD.Artist)
        (Maybe.fromMaybe "" $ maybeTagText MPD.Date)
        (maybeTagText MPD.MUSICBRAINZ_RELEASETRACKID)
        (maybeTagText MPD.MUSICBRAINZ_ALBUMID)
        (maybeTagText MPD.MUSICBRAINZ_ARTISTID)
        (fromInteger $ MPD.sgLength song)
          where
            tags = MPD.sgTags song
            maybeTagText name = fmap MPD.toText $ Map.lookup name tags >>= maybeHead
            tagOrDefault v name = Maybe.fromMaybe v $ Map.lookup name tags >>= maybeHead >>=
                  (Read.readMaybe . MPD.toString)

waitChange = MPD.idle [MPD.PlaylistS, MPD.PlayerS]

runMPD config act = do
  let withMPD = case config of
                  Nothing -> MPD.withMPD
                  (Just (Config.ConfigMPD host port Nothing)) ->
                    MPD.withMPD_ (Just host) (Just $ show port)
                  (Just (Config.ConfigMPD host port (Just password))) ->
                    MPD.withMPDEx host (fromIntegral port) password
  res <- MonadIO.liftIO $
    Exception.handle (\e ->
      let e' = e :: Exception.IOException
       in return (Left (Printf.printf "MPD error: %s" $ show e))) $ do
      resp <- withMPD act
      case resp of
        Left err -> return $ Left $ show err
        Right res -> return $ Right res
  case res of
    Left err -> Except.throwError $
      Printf.printf "MPD action failed: %s" (show err)
    Right r -> return r

makeTimestamp :: (MonadIO.MonadIO m, Except.MonadError String m) =>
  m Model.Timestamp
makeTimestamp = Model.Timestamp <$>
    currentSystemTime <*>
    currentUTCTime

currentSystemTime :: (MonadIO.MonadIO m, Except.MonadError String m) => m Clock.TimeSpec
currentSystemTime = MonadIO.liftIO $ Clock.getTime Clock.Monotonic

currentUTCTime ::(MonadIO.MonadIO m, Except.MonadError String m) => m UTCClock.UTCTime
currentUTCTime = MonadIO.liftIO $ UTCClock.getCurrentTime

maybeHead = Foldable.find (const True)
