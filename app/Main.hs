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

-- TODO: Probably want to move this somewhere...
applicationSettings :: MonadReader App m => m Settings
applicationSettings = do
  pure $
    Warp.defaultSettings
      & Warp.setPort 8081

-- TODO: Figure this out...
-- & Warp.setOnException Exception.onException

main :: IO ()
main = do
  configuration <- getConfiguration
  bracket (makeApp configuration) destroyApp $ \env -> do
    app' <- app env
    Warp.runSettings (runReader applicationSettings env) app'
