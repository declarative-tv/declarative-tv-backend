module App where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT)
import UnliftIO (MonadIO, MonadUnliftIO)

-- | Various app error types in the codebase
data AppError = AppError

-- | The application configuration
data App = App

-- | The application environment
newtype AppM a = AppM {runAppM :: ReaderT App (ExceptT AppError IO) a}
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader App
    , MonadError AppError
    )
