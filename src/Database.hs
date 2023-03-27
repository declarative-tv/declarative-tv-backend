module Database where

import App.Types
import Database.Esqueleto.Experimental
import Database.Model.Streamer
import My.Prelude

queryStreamers :: SqlQuery (SqlExpr (Entity Streamer))
queryStreamers = from $ table @Streamer

getStreamers :: MonadUnliftIO m => SqlPersistT m [Entity Streamer]
getStreamers = select queryStreamers

runDb :: (MonadReader App m, MonadUnliftIO m) => SqlPersistT m a -> m a
runDb query = do
  pool <- asks appPostgresPool
  runSqlPool query pool
