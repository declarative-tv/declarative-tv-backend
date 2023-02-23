module Api (app) where

import Api.Hello
import App
import Control.Monad.Except (ExceptT (..))
import My.Prelude
import Servant
import Servant.API.Generic (Generic)
import Servant.Server.Generic (AsServerT, genericServeT)

newtype Api route = Api
  { _api :: route :- "api" :> ToServant Hello AsApi
  }
  deriving (Generic)

apiImpl :: Api (AsServerT AppM)
apiImpl =
  Api
    { _api = toServant helloImpl
    }

app :: App -> IO Application
app config = pure $ genericServeT naturalTransformation apiImpl
  where
    naturalTransformation :: AppM a -> Handler a
    naturalTransformation appM =
      Handler . ExceptT . try . checkpoint "Api.handler" $
        runReaderT appM.runAppM config
