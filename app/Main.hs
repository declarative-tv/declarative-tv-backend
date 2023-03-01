module Main where

import Api (app)
import App
import Configuration
import Data.Function ((&))
import My.Prelude
import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Handler.Warp qualified as Warp
import UnliftIO (bracket)
import Warp.Exception qualified as Exception

warpSettings :: App -> Settings
warpSettings app =
  Warp.defaultSettings
    & Warp.setPort 8081
    & Warp.setOnException (\mReq se -> runReaderT (runAppM (Exception.onException mReq se)) app)
    & Warp.setOnExceptionResponse Exception.onExceptionResponse

main :: IO ()
main = do
  configuration <- getConfiguration
  bracket (makeApp configuration) destroyApp $ \env -> do
    app' <- app env
    Warp.runSettings (warpSettings env) app'
