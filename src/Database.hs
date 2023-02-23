module Database where

import App
import Data.Pool (
  withResource,
 )
import Database.PostgreSQL.Simple (
  Connection,
  Only (..),
  query_,
  withTransaction,
 )
import My.Prelude

connectionCount :: Connection -> IO Integer
connectionCount conn = do
  res :: [Only Integer] <- query_ conn "select count(*) from pg_stat_activity"
  pure $ case res of
    [] -> 0
    (x : _) -> fromOnly x

{- | Run a database action in a transaction. The postgresl-simple library is
 essentially in 'IO'. This is kind of wonky because this will run any 'IO'
 action. We could take a 'Query' and use `query :: (ToRow q, FromRow r) =>
 Connection -> Query -> q -> IO [r]`.
-}
runDb :: (MonadReader App m, MonadIO m) => (Connection -> IO a) -> m a
runDb ma = do
  pool <- appPostgresPool <$> ask
  liftIO $
    withResource pool $ \conn ->
      withTransaction conn $ do
        ma conn
