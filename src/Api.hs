module Api (app) where

import Api.Hello
import App
import Control.Monad.Reader (runReaderT)
import Servant
import Servant.API.Generic (Generic)
import Servant.Server.Generic (AsServerT, genericServeT)
import UnliftIO (Exception (fromException), liftIO, tryAny)

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
app config = pure $ genericServeT handler apiImpl
  where
    -- Attempt to handle 'ServerError's thrown by the application.
    -- Any other error is deemed "unexpected" and we'll not leak details.
    handler :: AppM a -> Handler a
    handler appM = do
      res <- liftIO . tryAny $ runReaderT (runAppM appM) config
      case res of
        Left exception -> do
          let mAppError = fromException @ServerError exception
          case mAppError of
            Just serverError ->
              throwError serverError
            Nothing -> do
              -- TODO: Log the exception
              liftIO $ print exception
              throwError err500 {errBody = "Unexpected server error..."}
        Right a -> pure a
