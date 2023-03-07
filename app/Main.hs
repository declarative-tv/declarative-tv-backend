module Main where

import Api (app)
import App
import Configuration
import Data.Function ((&))
import Database.PostgreSQL.Simple (ConnectInfo (..))
import My.Prelude
import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Handler.Warp qualified as Warp
import UnliftIO (bracket)
import Warp.Exception qualified as Exception

warpSettings :: App -> Settings
warpSettings application =
  Warp.defaultSettings
    & Warp.setPort 8081
    & Warp.setOnException (\mReq se -> runReaderT (runAppM (Exception.onException mReq se)) application)
    & Warp.setOnExceptionResponse Exception.onExceptionResponse

main :: IO ()
main = do
  configuration <- getConfiguration
  bracket (makeApp configuration) destroyApp $ \env -> do
    app' <- app env
    logIO env "hello" InfoS $ "Application starting on port: " <> lshow configuration.postgresConnectInfo.connectPort
    Warp.runSettings (warpSettings env) app'
