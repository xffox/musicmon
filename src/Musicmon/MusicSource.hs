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
    processState state

waitState :: (MonadIO.MonadIO m, Except.MonadError String m) =>
  Maybe Config.ConfigMPD -> m (Model.PlayerState, Model.Timestamp)
waitState config = do
  state <- runMPD config $ do
    waitChange
    makePlayerState
  processState state

processState state = do
    ts <- makeTimestamp
    case state of
        (Right st) -> 
            return (st, ts)
        (Left (MPD.Unexpected _)) ->
            return (Model.StateStopped, ts)
        Left err -> Except.throwError (Printf.printf "MPD action failed: %s" (show err))

makePlayerState = do
  st <- MPD.status
  case MPD.stState st of
    MPD.Stopped -> return $ Model.StateStopped
    MPD.Playing -> Model.StatePlaying <$> prepareSong st
    MPD.Paused -> Model.StatePaused <$> prepareSong st
  where prepareSong st = do
          maybeSong <- fmap maybeHead $ MPD.playlistId $ MPD.stSongID st
          case maybeSong of
            Just s -> return $ toModelSong s
            -- weird state with unknown songs on some server implementations
            _ -> return $ Model.Song "" 0 0 "" "" "" Nothing Nothing Nothing 0

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
       in return (Left ((Printf.printf "MPD error: %s" $ show e') :: String))) $ fmap Right $ withMPD act
  case res of
    Left err -> Except.throwError $ err
    Right r -> return r

makeTimestamp :: (MonadIO.MonadIO m, Except.MonadError String m) =>
  m Model.Timestamp
makeTimestamp = Model.Timestamp <$>
    currentSystemTime <*>
    currentUTCTime

currentSystemTime :: (MonadIO.MonadIO m, Except.MonadError String m) => m Clock.TimeSpec
currentSystemTime = MonadIO.liftIO $ Clock.getTime Clock.Monotonic

currentUTCTime ::(MonadIO.MonadIO m, Except.MonadError String m) => m UTCClock.UTCTime
currentUTCTime = MonadIO.liftIO UTCClock.getCurrentTime

maybeHead = Foldable.find (const True)
