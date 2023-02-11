module App where

import My.Prelude

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
