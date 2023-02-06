module Main where

import Api (app)
import App (App (..))
import Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  api' <- app App
  Warp.run 8081 api'
