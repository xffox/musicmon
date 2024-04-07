{-# LANGUAGE OverloadedStrings, TemplateHaskell, LambdaCase #-}
module Main where

import qualified Data.Text as Text
import qualified Data.List.Split as Split
import qualified Control.Monad as Monad
import qualified Text.Printf as Printf
import qualified Control.Monad.IO.Class as MonadIO
import qualified Conduit
import qualified Data.ConfigFile as ConfigFile
import qualified Database.Persist.Sql as PersistSql
import qualified Database.Persist.Sqlite as PersistSqlite
import qualified Control.Monad.Logger as Logger
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.Resource as Resource
import qualified System.Directory as Directory
import qualified Data.Conduit.TMChan as TMChan
import Control.Applicative((<|>))
import Conduit((.|))

import qualified Musicmon.Scrobble as Scrobble
import qualified Musicmon.Config as Config
import qualified Musicmon.Store as Store

dbSection = "db"
mpdSection = "mpd"
scrobbleSection = "scrobble"

fromCPError :: Functor m => Except.ExceptT ConfigFile.CPError m a ->
    Except.ExceptT String m a
fromCPError = Except.withExceptT snd

configGet :: (Monad m, ConfigFile.Get_C a) =>
    ConfigFile.ConfigParser ->
        ConfigFile.SectionSpec -> ConfigFile.OptionSpec ->
            Except.ExceptT String m a
configGet cp section option =
    fromCPError (ConfigFile.get cp section option)

configTryGet :: (Monad m, ConfigFile.Get_C a) =>
    ConfigFile.ConfigParser ->
        ConfigFile.SectionSpec -> ConfigFile.OptionSpec ->
            Except.ExceptT String m (Maybe a)
configTryGet cp section option =
    fromCPError $ (Just <$> ConfigFile.get cp section option) `Except.catchError`
        \case (ConfigFile.NoOption _, _) -> return Nothing
              e -> Except.throwError e

readDBConfig :: Monad m => ConfigFile.ConfigParser ->
    Except.ExceptT String m (Maybe Config.ConfigDB)
readDBConfig cp = do
    if ConfigFile.has_section cp dbSection
       then Just . Config.ConfigDB <$> configGet cp dbSection "filename"
       else return Nothing

readMPDConfig cp = do
    if ConfigFile.has_section cp mpdSection
       then ((:[]) . Just <$> readMPDSection cp mpdSection) <|>
           readMPDMultipleSections cp mpdSection
       else return [Nothing]

readMPDSection cp section = do
    Config.ConfigMPD <$>
        configGet cp section "host" <*>
        configGet cp section "port" <*>
        configTryGet cp section "password"

readMPDMultipleSections cp mainSection = do
    sections <- Split.splitOn "," <$> configGet cp mainSection "servers"
    Monad.mapM (fmap Just . readMPDSection cp) sections

readScrobbleConfig cp = do
    Monad.unless (ConfigFile.has_section cp scrobbleSection) $
        Except.throwError "scrobble"
    Config.ConfigScrobble <$>
        configGet cp scrobbleSection "minplayed"

readConfig :: IO (Either String Config.Config)
readConfig =
    Except.runExceptT $ do
        workdir <- MonadIO.liftIO $
            Directory.getXdgDirectory Directory.XdgConfig "musicmon"
        MonadIO.liftIO $ Directory.setCurrentDirectory workdir
        cp <- fromCPError $ Monad.join $ MonadIO.liftIO $
            ConfigFile.readfile ConfigFile.emptyCP "musicmon.cfg"
        Config.Config <$> readDBConfig cp <*> readMPDConfig cp <*>
            readScrobbleConfig cp

runScrobble scrobbleConfig mpdConfigs sink = do
    let produceSongs mpdConfig =
            Scrobble.produceStates
              (mpdConfig, scrobbleConfig) .|
                  Scrobble.filterSongs scrobbleConfig
    Resource.runResourceT $ do
        sources <- Conduit.runResourceT
            (TMChan.mergeSources
                (map produceSongs mpdConfigs) (max 1 (length mpdConfigs)))
        Conduit.runConduit $ sources .|
            Conduit.iterMC $(Logger.logInfoSH) .| sink

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
              $(Logger.logInfo) $ Text.pack $
                  Printf.printf "mpd connections: %s" (show $ Config.configMPD config)
              $(Logger.logInfo) $ Text.pack $
                  Printf.printf "db: %s" (show $ Config.configDB config)
              case Config.configDB config of
                Just dbConfig ->
                    PersistSqlite.withSqliteConn (Text.pack $
                        Config.configDBFilename dbConfig) $
                      \db -> do
                          flip PersistSql.runSqlConn db $
                              PersistSqlite.runMigrationSilent Store.migrateTables
                          runScrobble (Config.configScrobble config)
                            (Config.configMPD config) (Store.sinkDatabase db)
                          return ()
                Nothing -> runScrobble (Config.configScrobble config)
                    (Config.configMPD config) Conduit.sinkNull
