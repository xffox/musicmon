{-# LANGUAGE OverloadedStrings #-}
module MusicStateSpec where

import Test.Hspec
import qualified Data.Maybe as Maybe
import qualified Control.Monad as Monad
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Calendar.OrdinalDate as OrdinalDate
import qualified System.Clock as SystemClock

import qualified Musicmon.Model as Model
import qualified Musicmon.Config as Config
import qualified Musicmon.MusicState as MusicState

startTime = Model.Timestamp
    (SystemClock.TimeSpec 0 0)
    (Clock.UTCTime
        (OrdinalDate.fromOrdinalDate 2015 42)
        (Clock.secondsToDiffTime 0))

timeAddSeconds (Model.Timestamp (SystemClock.TimeSpec sec nsec) utcTime) diffSeconds =
    Model.Timestamp (SystemClock.TimeSpec (sec + diffSeconds) nsec)
        (Clock.addUTCTime (Clock.secondsToNominalDiffTime $ fromIntegral diffSeconds) utcTime)

prepareSong title length = Model.Song title
    1 1 "" "" "" Nothing Nothing Nothing length

shouldProduce playerStates expSongs = do
    (Maybe.catMaybes $ map snd $ reverse $ foldl step [] playerStates)
         `shouldBe` expSongs
     where step prevs ps = (MusicState.stepMusicState (state prevs) ps):prevs
           state [] = MusicState.initMusicState (Config.ConfigScrobble 0.5)
           state ((st,_):_) = st

spec = describe "music state" $ do
        it "yields a song when enough is played" $ do
          let song = prepareSong "Painkiller" 15
              ts = timeAddSeconds startTime 8
          [(Model.StatePlaying song, startTime),
           (Model.StateStopped, ts)] `shouldProduce`
             [Model.PlayedSong ts song]
        it "doesn't yield a song when not enough is played" $ do
          let song = prepareSong "Painkiller" 15
          [(Model.StatePlaying song, startTime),
           (Model.StateStopped, timeAddSeconds startTime 6)] `shouldProduce`
             []
        it "yields a paused and stopped song" $ do
          let song = prepareSong "Painkiller" 15
          [(Model.StatePlaying song, startTime),
           (Model.StatePaused song, timeAddSeconds startTime 8),
           (Model.StateStopped, timeAddSeconds startTime 13)] `shouldProduce`
             [Model.PlayedSong (timeAddSeconds startTime 13) song]
        it "yields multiple songs" $ do
          let song1 = prepareSong "Painkiller" 15
              song2 = prepareSong "Symbolic" 16
          [(Model.StatePlaying song1, startTime),
           (Model.StatePlaying song2, timeAddSeconds startTime 8),
           (Model.StateStopped, timeAddSeconds startTime 17)] `shouldProduce`
             [Model.PlayedSong (timeAddSeconds startTime 8) song1,
              Model.PlayedSong (timeAddSeconds startTime 17) song2]
        it "yields paused and continued song" $ do
          let song = prepareSong "Painkiller" 15
          [(Model.StatePlaying song, startTime),
           (Model.StatePaused song, timeAddSeconds startTime 3),
           (Model.StatePlaying song, timeAddSeconds startTime 4),
           (Model.StateStopped, timeAddSeconds startTime 9)] `shouldProduce`
             [Model.PlayedSong (timeAddSeconds startTime 9) song]
        it "doesn't yield a paused song when not enough is played" $ do
          let song = prepareSong "Painkiller" 15
          [(Model.StatePlaying song, startTime),
           (Model.StatePaused song, timeAddSeconds startTime 3),
           (Model.StatePlaying song, timeAddSeconds startTime 15),
           (Model.StateStopped, timeAddSeconds startTime 18)] `shouldProduce`
             []
