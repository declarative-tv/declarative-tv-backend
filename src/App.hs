{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App where

import Configuration
import Data.Pool (
  Pool,
  createPool,
  destroyAllResources,
 )
import Database.PostgreSQL.Simple (
  ConnectInfo (..),
  Connection,
  close,
  connect,
 )
import My.Prelude

-- | The application environment following the "ReaderT over IO" pattern
newtype AppM a = AppM {runAppM :: ReaderT (App AppM) IO a}
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader (App AppM)
    , MonadThrow
    )

{- | A 'ConstraintKind' describing our application's context,

 * 'MonadIO' for running database actions and various 'IO' actions
 * 'MonadReader (App AppM)' so we can get 'App' via 'ask'
 * 'MonadThrow' so we can use 'throwM'
 * 'WithLog (App AppM) Message' so we can 'log' with co-log
-}
type AppContext m = (MonadIO m, MonadReader (App AppM) m, MonadThrow m, WithLog (App AppM) Message m)

{- | The application's environment

 * 'appLogAction' is our co-log environment
 * 'appPostgresPool' is our instantiated postgresql pool to get connections from
-}
data App m = App
  { appLogAction :: !(LogAction m Message)
  , appPostgresPool :: Pool Connection
  }

-- | Comes from co-log documentation, allows us to use 'WithLog (App AppM) Message m' in 'AppContext'.
instance HasLog (App m) Message m where
  getLogAction :: App m -> LogAction m Message
  getLogAction = appLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> App m -> App m
  setLogAction newLogAction app = app {appLogAction = newLogAction}
  {-# INLINE setLogAction #-}

-- | Generate the connection pool from the connection information
makePostgresPool :: ConnectInfo -> IO (Pool Connection)
makePostgresPool connectInfo =
  checkpoint "makePostgresPool" $
    createPool (connect connectInfo) close 1 10 10

-- | Generate our application environment
makeApp :: Configuration -> IO (App AppM)
makeApp Configuration {..} = do
  appPostgresPool <- makePostgresPool postgresConnectInfo
  pure
    App
      { appLogAction = richMessageAction
      , ..
      }

-- | Tear down our application environment
destroyApp :: App AppM -> IO ()
destroyApp App {..} =
  destroyAllResources appPostgresPool
