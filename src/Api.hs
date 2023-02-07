module Api (app) where

import Api.Hello
import App
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Text (Text)
import Servant
import Servant.API.Generic (Generic)
import Servant.Server.Generic (AsServerT, genericServeT)
import UnliftIO (liftIO, tryAny)

newtype Api route = Api
  { _api :: route :- "api" :> ToServant Hello AsApi
  }
  deriving (Generic)

api :: Proxy (ToServantApi Api)
api = genericApi (Proxy :: Proxy Api)

apiImpl :: Api (AsServerT AppM)
apiImpl =
  Api
    { _api = toServant helloImpl
    }

app :: App -> IO Application
app config = pure $ genericServeT handler apiImpl
  where
    handler :: AppM a -> Handler a
    handler appM = do
      res <-
        -- Is 'tryAny' really want we want here?
        liftIO . tryAny $ runReaderT (runAppM appM) config
      case res of
        Left err -> do
          liftIO $ print err
          -- TODO: Log the exception
          throwError err500 {errBody = "Unexpected server error..."}
        Right a -> pure a
