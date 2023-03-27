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
  streamers <- runDb getStreamers
  logMsg "hello" InfoS $ "Hit /hello/work, with count " <> lshow streamers
  pure "hello, world"

failAction :: AppContext m => m Text
failAction = do
  streamers <- runDb getStreamers
  logMsg "hello" InfoS $ "Hit /hello/fail, with count " <> lshow streamers
  throwM $ err401 {errBody = "This action failed..."}
