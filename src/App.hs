module App where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT)
import UnliftIO (MonadIO)

-- | The application configuration
data App = App

-- | The application environment
newtype AppM a = AppM {runAppM :: ReaderT App IO a}
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader App
    , MonadThrow
    )
