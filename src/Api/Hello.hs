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

workAction :: MonadReader App m => m Text
workAction = pure "hello, world"

failAction :: (MonadThrow m, MonadReader App m) => m Text
failAction = throwM $ err401 {errBody = "This action failed..."}
