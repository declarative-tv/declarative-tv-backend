module Main where

import Api (app)
import App (App (..))
import Data.Pool (
  Pool,
  createPool,
  destroyAllResources,
  withResource,
 )
import Database.PostgreSQL.Simple (
  ConnectInfo (..),
  Connection,
  Only (..),
  close,
  connect,
  defaultConnectInfo,
  query_,
 )
import Network.Wai.Handler.Warp qualified as Warp
import UnliftIO (bracket)

connectionInfo :: ConnectInfo
connectionInfo =
  defaultConnectInfo
    { connectHost = "localhost"
    , connectPort = 5432
    , connectUser = "chiroptical"
    , connectDatabase = "declarative-tv"
    }

pgPool :: IO (Pool Connection)
pgPool = createPool (connect connectionInfo) close 1 10 10

connectionCount :: Pool Connection -> IO Integer
connectionCount pool =
  withResource pool $ \conn -> do
    res :: [Only Integer] <- query_ conn "select count(*) from pg_stat_activity"
    pure . fromOnly $ head res

main :: IO ()
main = do
  bracket pgPool destroyAllResources $ \pool -> do
    count <- connectionCount pool
    print $ "Current connection count " <> show count
    app' <- app App
    Warp.run 8081 app'
