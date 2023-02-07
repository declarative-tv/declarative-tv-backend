module Main where

import Api (app)
import App (App (..))
import Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  app' <- app App
  Warp.run 8081 app'
