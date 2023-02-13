{-# LANGUAGE AllowAmbiguousTypes #-}

module Api.Hello where

import App
import Database
import My.Prelude
import Servant
import Servant.API.Generic (Generic)
import Servant.Server.Generic (AsServerT)

data Hello route = Hello
  { helloFail :: route :- "hello" :> "fail" :> Get '[JSON] Text
  , helloWork :: route :- "hello" :> "work" :> Get '[JSON] Text
  }
  deriving (Generic)

helloImpl :: Hello (AsServerT AppM)
helloImpl =
  Hello
    { helloFail = failAction
    , helloWork = workAction
    }

workAction :: AppContext m => m Text
workAction = do
  count <- runDb connectionCount
  log I $ "Hit /hello/work, with count " <> tshow count
  pure "hello, world"

failAction :: AppContext m => m Text
failAction = do
  count <- runDb connectionCount
  log E $ "Hit /hello/fail, with count " <> tshow count
  throwM $ err401 {errBody = "This action failed..."}
