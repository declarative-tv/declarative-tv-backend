{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
import GHC.Stack (HasCallStack)
import My.Prelude
import UnliftIO (stdout)

-- | The application environment following the "ReaderT over IO" pattern
newtype AppM a = AppM {runAppM :: ReaderT App IO a}
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader App
    , MonadThrow
    )

{- | A 'ConstraintKind' describing our application's context,

 * 'MonadIO' for running database actions and various 'IO' actions
 * 'MonadReader App' so we can get 'App' via 'ask'
 * 'MonadThrow' so we can use 'throwM'
 * 'Katip' for logging
-}
type AppContext m = (MonadIO m, MonadReader App m, MonadThrow m, Katip m, HasCallStack)

{- | The application's environment

 * 'appLogNamespace' contains the current katip namespace
 * 'appLogContext' contains the current katip context
 * 'appLogEnv' contains the katip environment that is constructed in 'makeApp'
 * 'appPostgresPool' is our instantiated postgresql pool to get connections from
-}
data App = App
  { appLogNamespace :: Namespace
  , appLogContext :: LogContexts
  , appLogEnv :: LogEnv
  , appPostgresPool :: Pool Connection
  }

instance Katip AppM where
  getLogEnv = asks appLogEnv
  localLogEnv f (AppM m) = AppM (local (\s -> s {appLogEnv = f s.appLogEnv}) m)

instance KatipContext AppM where
  getKatipContext = asks appLogContext
  localKatipContext f (AppM m) = AppM (local (\s -> s {appLogContext = f s.appLogContext}) m)

  getKatipNamespace = asks appLogNamespace
  localKatipNamespace f (AppM m) = AppM (local (\s -> s {appLogNamespace = f s.appLogNamespace}) m)

-- | Generate the connection pool from the connection information
makePostgresPool :: ConnectInfo -> IO (Pool Connection)
makePostgresPool connectInfo =
  checkpoint "makePostgresPool" $
    createPool (connect connectInfo) close 1 10 10

-- | Generate our application environment
makeApp :: Configuration -> IO App
makeApp Configuration {..} = do
  appPostgresPool <- makePostgresPool postgresConnectInfo
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  appLogEnv <-
    registerScribe "stdout" handleScribe defaultScribeSettings
      =<< initLogEnv "declarative.tv" "production"
  pure
    App
      { appLogNamespace = mempty
      , appLogContext = mempty
      , ..
      }

-- | Tear down our application environment
destroyApp :: App -> IO ()
destroyApp App {..} = do
  destroyAllResources appPostgresPool
  void $ closeScribes appLogEnv
