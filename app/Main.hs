module Main where

import Api (app)
import App
import Configuration
import My.Prelude
import Network.Wai.Handler.Warp qualified as Warp
import UnliftIO (bracket)

main :: IO ()
main = do
  configuration <- getConfiguration
  bracket (makeApp configuration) destroyApp $ \env -> do
    app' <- app env
    Warp.run 8081 app'
