{-# LANGUAGE FlexibleContexts #-}

module Api.Hello where

import App
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

workAction :: WithLog env Message m => m Text
workAction = do
  log I "Hit /hello/work"
  pure "hello, world"

failAction :: (WithLog env Message m, MonadThrow m) => m Text
failAction = do
  log E "Hit /hello/fail"
  throwM $ err401 {errBody = "This action failed..."}
