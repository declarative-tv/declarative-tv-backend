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
  logMsg "hello" InfoS $ "Hit /hello/work, with count " <> lshow count
  pure "hello, world"

failAction :: AppContext m => m Text
failAction = do
  count <- runDb connectionCount
  logMsg "hello" InfoS $ "Hit /hello/fail, with count " <> lshow count
  throwM $ err401 {errBody = "This action failed..."}
