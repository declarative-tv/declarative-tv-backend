module Api (app) where

import Api.Hello
import App
import GHC.Stack (callStack)
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

app :: App AppM -> IO Application
app config = pure $ genericServeT handler apiImpl
  where
    -- Attempt to handle 'ServerError's thrown by the application.
    -- Any other error is deemed "unexpected" and we'll not leak details.
    handler :: AppM a -> Handler a
    handler appM = do
      res <-
        liftIO . try . checkpoint "Api.handler" $
          runReaderT appM.runAppM config
      case res of
        Left exception -> do
          let mAnnotatedException = fromException @(AnnotatedException ServerError) exception
          case mAnnotatedException of
            Just annotatedException ->
              throwError annotatedException.exception
            Nothing -> do
              -- TODO: Is there a better way? It would be nice to have this
              -- formatted the same as my other log messages.
              liftIO . print . fmtMessage . Msg Error callStack $ tshow exception
              throwError err500 {errBody = "Unexpected server error..."}
        Right a -> pure a
