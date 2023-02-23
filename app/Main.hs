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
    -- TODO: move exception handling to the warp level
    -- see https://hackage.haskell.org/package/warp-3.3.24/docs/Network-Wai-Handler-Warp.html#v:setOnException
    Warp.run 8081 app'
