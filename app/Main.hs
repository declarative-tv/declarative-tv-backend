module Main where

import Api (app)
import App
import App.Types
import Configuration
import Data.Function ((&))
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
    logIO env "starting" InfoS $
      "Application starting with"
        <> " postgresHost: "
        <> lshow configuration.postgresConnectInfo.postgresHost
        <> " postgresPost: "
        <> lshow configuration.postgresConnectInfo.postgresPort
        <> " postgresDatabaseName: "
        <> lshow configuration.postgresConnectInfo.postgresDatabaseName
        <> " postgresUser: "
        <> lshow configuration.postgresConnectInfo.postgresUser
    Warp.runSettings (warpSettings env) app'
