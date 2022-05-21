{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Musicmon.Scrobble (produceStates, filterSongs) where

import qualified Data.Text as T
import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.Except as Except
import qualified Text.Printf as Printf
import qualified Conduit
import qualified Control.Monad.Logger as Logger

import qualified Musicmon.Model as Model
import qualified Musicmon.Config as Config
import qualified Musicmon.MusicState as MusicState
import qualified Musicmon.MusicSource as MusicSource

produceStates :: (MonadIO.MonadIO m, MonadFail m, Logger.MonadLogger m) =>
  Config.Config -> Conduit.ConduitT i (Maybe (Model.PlayerState, Model.Timestamp)) m ()
produceStates config = do
  st <- Except.runExceptT $ MusicSource.getState (Config.configMPD config)
  handleState st
  where doProduceStates = do
            st <- Except.runExceptT $ MusicSource.waitState (Config.configMPD config)
            handleState st
        handleState (Right s) = do
            Conduit.yield $ Just s
            doProduceStates
        handleState s@(Left err) = do
            Conduit.yield Nothing
            $(Logger.logError) $ T.pack $
              Printf.printf "source failed: %s" (show err)
            MonadIO.liftIO $ Concurrent.threadDelay (5*10^6)
            produceStates config

filterSongs :: (MonadIO.MonadIO m, MonadFail m, Logger.MonadLogger m) =>
  Config.Config ->
  Conduit.ConduitT (Maybe (Model.PlayerState, Model.Timestamp)) Model.PlayedSong m ()
filterSongs config = doFilterSongs (MusicState.initMusicState (Config.configScrobble config))
    where doFilterSongs state = do
            cur <- Conduit.await
            case cur of
              Just (Just ps) ->
                let (state', song) = MusicState.stepMusicState state ps
                 in do
                   case song of
                     Just s -> Conduit.yield s
                     _ -> return ()
                   doFilterSongs state'
              Just Nothing -> doFilterSongs (MusicState.resetMusicState state)
              Nothing -> return ()
