{-# LANGUAGE TupleSections #-}
module Musicmon.MusicState(
  MusicState,
  initMusicState, stepMusicState, resetMusicState
  ) where
import qualified Control.Arrow as Arrow
import qualified System.Clock as SystemClock
import qualified Musicmon.Model as Model
import qualified Musicmon.Config as Config

data MusicState =
  MusicState Config.ConfigScrobble (Maybe (SongState, Model.SystemTime))

data SongState = SongState {
    songPlayerState :: Model.PlayerState,
    songPlayedDuration :: Model.Duration
                             }

initMusicState :: Config.ConfigScrobble -> MusicState
initMusicState config = MusicState config Nothing

resetMusicState :: MusicState -> MusicState
resetMusicState (MusicState config _) = MusicState config Nothing

stepMusicState :: MusicState -> (Model.PlayerState, Model.Timestamp) ->
  (MusicState, Maybe Model.PlayedSong)
stepMusicState (MusicState config Nothing) (playerState, time) =
  (MusicState config (Just (SongState playerState 0, Model.timestampSystem time)),
  Nothing)
stepMusicState musicState@(MusicState config (Just (songState, prevTime))) (playerState, time)
      | songPlayerState songState /= playerState =
        let systemTime = Model.timestampSystem time
            duration = timeDuration systemTime prevTime
         in Arrow.first (Arrow.arr ((MusicState config) . Just . (,systemTime))) $
           stepDuration songState playerState duration
      | otherwise = (musicState, Nothing)
    where
      stepDuration songState playerState duration =
        let totalDuration = songPlayedDuration songState + duration
         in case songPlayerState songState of
            (Model.StatePlaying prevSong) ->
              case playerState of
                Model.StatePaused songID | songID == prevSong ->
                  (SongState playerState totalDuration, Nothing)
                _ ->
                  (SongState playerState 0,
                  fmap (Model.PlayedSong time) $
                    checkPlayedDuration config prevSong totalDuration)
            (Model.StatePaused prevSong) ->
              case playerState of
                Model.StatePlaying songID | songID == prevSong ->
                  (SongState playerState (songPlayedDuration songState), Nothing)
                _ ->
                  (SongState playerState 0,
                  fmap (Model.PlayedSong time) $
                    checkPlayedDuration config prevSong (songPlayedDuration songState))
            Model.StateStopped ->
                  (SongState playerState 0, Nothing)

checkPlayedDuration configScrobble song playedDuration
  | (Model.songDuration song /= 0) &&
      (almostGreaterEqual playedDuration
        (Model.songDuration song * Config.scrobbleMinPlayed configScrobble)) =
          Just song
  | otherwise = Nothing

almostGreaterEqual left right = (left - right) >= -delta
  where delta = 1

timeDuration cur prev =
  nanoSecsToSecs $ SystemClock.toNanoSecs $ SystemClock.diffTimeSpec cur prev

nanoSecsToSecs = (/(10^9)) . fromIntegral
