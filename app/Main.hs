{-# LANGUAGE OverloadedStrings, TemplateHaskell, LambdaCase #-}
module Main where

import qualified Data.Text as Text
import qualified Control.Monad as Monad
import qualified Text.Printf as Printf
import qualified Control.Monad.IO.Class as MonadIO
import qualified Conduit
import qualified Data.ConfigFile as ConfigFile
import qualified Database.Persist.Sql as PersistSql
import qualified Database.Persist.Sqlite as PersistSqlite
import qualified Control.Monad.Logger as Logger
import qualified Control.Monad.Except as Except
import qualified System.Directory as Directory
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
    Except.ExceptT String m Config.ConfigDB
readDBConfig cp = do
    Monad.unless (ConfigFile.has_section cp dbSection) $ Except.throwError "db"
    Config.ConfigDB <$>
        configGet cp dbSection "filename"

readMPDConfig cp = do
    if ConfigFile.has_section cp mpdSection
       then fmap Just $ Config.ConfigMPD <$>
           configGet cp mpdSection "host" <*>
           configGet cp mpdSection "port" <*>
           configTryGet cp mpdSection "password"
       else return Nothing

readScrobbleConfig cp = do
    Monad.unless (ConfigFile.has_section cp scrobbleSection) $
        Except.throwError "scrobble"
    Config.ConfigScrobble <$>
        configGet cp scrobbleSection "minplayed"

readConfig :: IO (Either String Config.Config)
readConfig =
    Except.runExceptT $ do
        workdir <- Except.liftIO $
            Directory.getXdgDirectory Directory.XdgConfig "musicmon"
        Except.liftIO $ Directory.setCurrentDirectory workdir
        cp <- fromCPError $ Except.join $ Except.liftIO $
            ConfigFile.readfile ConfigFile.emptyCP "musicmon.cfg"
        Config.Config <$> readDBConfig cp <*> readMPDConfig cp <*>
            readScrobbleConfig cp

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
                  Printf.printf "mpd connection: %s" (show $ Config.configMPD config)
              PersistSqlite.withSqliteConn (Text.pack $
                    Config.configDBFilename $ Config.configDB config) $
                  \db -> do
                      flip PersistSql.runSqlConn db $
                          PersistSqlite.runMigrationSilent Store.migrateTables
                      Conduit.runConduit $
                          Scrobble.produceStates config .|
                              Scrobble.filterSongs config .|
                              Conduit.iterMC $(Logger.logInfoSH) .|
                                  Store.sinkDatabase db
