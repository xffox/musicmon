module Musicmon.Model(
  Timestamp(..), Song(..), PlayerState(..), PlayedSong(..),
  Duration, SystemTime, UTCTime
                     ) where

import qualified Data.Text as Text
import qualified System.Clock as SystemClock
import qualified Data.Time.Clock as Clock

type Duration = Float
type SystemTime = SystemClock.TimeSpec
type UTCTime = Clock.UTCTime

data PlayerState =
      StateStopped |
      StatePaused Song |
      StatePlaying Song
    deriving (Eq, Show)

data PlayedSong = PlayedSong Timestamp Song
    deriving (Eq, Show)

data Timestamp = Timestamp {
timestampSystem :: SystemTime,
timestampUTC :: UTCTime
               }
               deriving (Eq, Show)

data Song = Song {
songTitle :: Text.Text,
songTrack :: Integer,
songDisc :: Integer,
songAlbum :: Text.Text,
songArtist :: Text.Text,
songDate :: Text.Text,
songMBReleaseTrackID :: Maybe Text.Text,
songMBAlbumID :: Maybe Text.Text,
songMBArtistID :: Maybe Text.Text,
songDuration :: Duration
                 }
                 deriving (Eq, Show)
