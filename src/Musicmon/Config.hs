module Musicmon.Config(Config(..),
  ConfigDB(..),
  ConfigMPD(..),
  ConfigScrobble(..)) where

newtype ConfigDB = ConfigDB {
    configDBFilename :: String
                            }
  deriving (Show)

data ConfigMPD = ConfigMPD {
    configMPDHost :: String,
    configMPDPort :: Int,
    configMPDPassword :: Maybe String
                           }
  deriving (Show)

newtype ConfigScrobble = ConfigScrobble {
  scrobbleMinPlayed :: Float
                                        }

data Config = Config {
    configDB :: Maybe ConfigDB,
    configMPD :: [Maybe ConfigMPD],
    configScrobble :: ConfigScrobble
                     }
