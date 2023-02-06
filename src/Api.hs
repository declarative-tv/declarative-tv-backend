module Api (app) where

import App
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Text (Text)
import Servant
import Servant.API.Generic (
  Generic,
 )
import Servant.Server.Generic (AsServerT, genericServeT)
import UnliftIO (liftIO)

newtype Api route = Api
  { _hello :: route :- "hello" :> Get '[JSON] Text
  }
  deriving (Generic)

api :: Proxy (ToServantApi Api)
api = genericApi (Proxy :: Proxy Api)

apiImpl :: Api (AsServerT AppM)
apiImpl =
  Api
    { _hello = pure "hello, world!"
    }

app :: App -> IO Application
app config = pure $ genericServeT handler apiImpl
  where
    handler :: AppM a -> Handler a
    handler appM = do
      res <- liftIO . runExceptT $ runReaderT (runAppM appM) config
      Handler . ExceptT . pure $ case res of
        Left AppError -> Left err500
        Right a -> Right a
