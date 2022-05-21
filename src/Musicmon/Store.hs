{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies,
   OverloadedStrings, GADTs, FlexibleContexts, DerivingStrategies,
   StandaloneDeriving, GeneralizedNewtypeDeriving, UndecidableInstances,
   DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
module Musicmon.Store(sinkDatabase, migrateTables) where

import qualified Data.Void as Void
import qualified Data.Text as Text
import qualified Data.Time.Clock as Clock
import qualified Conduit
import qualified Database.Persist.Sql as PersistSql
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
    share, sqlSettings)

import qualified Musicmon.Model as Model

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
    Song
        timestamp Clock.UTCTime
        title Text.Text
        track Int default=0
        disc Int default=0
        album Text.Text
        artist Text.Text
        date Text.Text
        mbReleaseTrackId Text.Text Maybe
        mbAlbumId Text.Text Maybe
        mbArtistId Text.Text Maybe
        |]

sinkDatabase :: (Monad m, Conduit.MonadUnliftIO m) => PersistSql.SqlBackend ->
    Conduit.ConduitT Model.PlayedSong Void.Void m ()
sinkDatabase db = do
    maybeSong <- Conduit.await
    case maybeSong of
      Nothing -> return ()
      Just song -> do
          Conduit.lift $ flip PersistSql.runSqlConn db $
              PersistSql.insert_ $ fromModelSong song
          sinkDatabase db

fromModelSong (Model.PlayedSong timestamp song) =
    Song (Model.timestampUTC timestamp)
        (Model.songTitle song)
        (fromIntegral $ Model.songTrack song)
        (fromIntegral $ Model.songDisc song)
        (Model.songAlbum song)
        (Model.songArtist song)
        (Model.songDate song)
        (Model.songMBReleaseTrackID song)
        (Model.songMBAlbumID song)
        (Model.songMBArtistID song)
