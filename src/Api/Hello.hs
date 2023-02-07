{-# LANGUAGE FlexibleContexts #-}

module Api.Hello where

import App
import Control.Exception (Exception, catch)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Reader (MonadReader)
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
    { hello = action
    }

action :: (MonadThrow m, MonadReader App m) => m Text
action = throwM $ err401 {errBody = "This action failed..."}
