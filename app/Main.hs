{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Conduit ((.|))
import qualified Conduit
import Control.Applicative ((<|>))
import qualified Control.Monad as ControlMonad
import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Logger as Logger
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Conduit.TMChan as TMChan
import qualified Data.Ini.Config as IniConfig
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Database.Persist.Sql as PersistSql
import qualified Database.Persist.Sqlite as PersistSqlite
import qualified System.Directory as Directory
import qualified Text.Printf as Printf

import qualified Musicmon.Config as Config
import qualified Musicmon.Scrobble as Scrobble
import qualified Musicmon.Store as Store

dbSection = "db"
mpdSection = "mpd"
scrobbleSection = "scrobble"

readDBConfig :: IniConfig.IniParser (Maybe Config.ConfigDB)
readDBConfig = do
    IniConfig.sectionMb dbSection $ do
        Config.ConfigDB <$> IniConfig.fieldOf "filename" IniConfig.string

readMPDConfig :: IniConfig.IniParser [Maybe Config.ConfigMPD]
readMPDConfig = do
    maybeServers <- IniConfig.sectionMb mpdSection $ do
        IniConfig.fieldOf "servers" (IniConfig.listWithSeparator "," IniConfig.string)
    case maybeServers of
        (Just servers) -> ((ControlMonad.mapM (fmap Just . readMPDSection)) servers)
        Nothing -> return [Nothing]

readMPDSection :: Text.Text -> IniConfig.IniParser Config.ConfigMPD
readMPDSection section = do
    IniConfig.section section $ do
        Config.ConfigMPD
            <$> IniConfig.fieldOf "host" IniConfig.string
            <*> IniConfig.fieldOf "port" IniConfig.number
            <*> IniConfig.fieldMbOf "password" IniConfig.string

readScrobbleConfig :: IniConfig.IniParser Config.ConfigScrobble
readScrobbleConfig = do
    IniConfig.section scrobbleSection $ do
        Config.ConfigScrobble <$> IniConfig.fieldOf "minplayed" IniConfig.readable

parseConfig :: IniConfig.IniParser Config.Config
parseConfig = do
    Config.Config <$> readDBConfig <*> readMPDConfig <*> readScrobbleConfig

readConfig :: IO (Either String Config.Config)
readConfig = do
    workdir <-
        MonadIO.liftIO $
            Directory.getXdgDirectory Directory.XdgConfig "musicmon"
    MonadIO.liftIO $ Directory.setCurrentDirectory workdir
    content <- TextIO.readFile "musicmon.cfg"
    return $ IniConfig.parseIniFile content parseConfig

runScrobble scrobbleConfig mpdConfigs sink = do
    let produceSongs mpdConfig =
            Scrobble.produceStates
                (mpdConfig, scrobbleConfig)
                .| Scrobble.filterSongs scrobbleConfig
    Resource.runResourceT $ do
        sources <-
            Conduit.runResourceT
                ( TMChan.mergeSources
                    (map produceSongs mpdConfigs)
                    (max 1 (length mpdConfigs))
                )
        Conduit.runConduit $
            sources
                .| Conduit.iterMC $(Logger.logInfoSH)
                .| sink

main :: IO ()
main = Logger.runStderrLoggingT $
    Logger.filterLogger (curry ((>= Logger.LevelInfo) . snd)) $ do
        maybeConf <- MonadIO.liftIO readConfig
        case maybeConf of
            Left err -> do
                $(Logger.logError) $ Text.concat ["invalid config: ", Text.pack err]
                return ()
            Right config -> do
                $(Logger.logInfo) "running music monitor"
                $(Logger.logInfo) $
                    Text.pack $
                        Printf.printf "mpd connections: %s" (show $ Config.configMPD config)
                $(Logger.logInfo) $
                    Text.pack $
                        Printf.printf "db: %s" (show $ Config.configDB config)
                case Config.configDB config of
                    Just dbConfig ->
                        PersistSqlite.withSqliteConn
                            ( Text.pack $
                                Config.configDBFilename dbConfig
                            )
                            $ \db -> do
                                flip PersistSql.runSqlConn db $
                                    PersistSqlite.runMigrationSilent Store.migrateTables
                                runScrobble
                                    (Config.configScrobble config)
                                    (Config.configMPD config)
                                    (Store.sinkDatabase db)
                                return ()
                    Nothing ->
                        runScrobble
                            (Config.configScrobble config)
                            (Config.configMPD config)
                            Conduit.sinkNull
