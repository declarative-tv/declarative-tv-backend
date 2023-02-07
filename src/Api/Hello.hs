module Api.Hello where

import App (AppM)
import Data.Text (Text)
import Servant
import Servant.API.Generic (Generic)
import Servant.Server.Generic (AsServerT)

newtype Hello route = Hello
  { hello :: route :- "hello" :> Get '[JSON] Text
  }
  deriving (Generic)

helloImpl :: Hello (AsServerT AppM)
helloImpl =
  Hello
    { hello = pure "hello, world!"
    }
