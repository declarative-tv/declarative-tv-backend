{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App where

import App.Types
import Configuration
import Control.Monad.Logger (runNoLoggingT)
import Data.Pool (Pool, destroyAllResources)
import Database.Migrate (migrateAll)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
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
    , MonadUnliftIO
    , MonadReader App
    , MonadThrow
    )

{- | A 'ConstraintKind' describing our application's context,

 * 'MonadIO' for running database actions and various 'IO' actions
 * 'MonadReader App' so we can get 'App' via 'ask'
 * 'MonadThrow' so we can use 'throwM'
 * 'Katip' for logging
-}
type AppContext m = (MonadUnliftIO m, MonadReader App m, MonadThrow m, Katip m, HasCallStack)

instance Katip AppM where
  getLogEnv = asks appLogEnv
  localLogEnv f (AppM m) = AppM (local (\s -> s {appLogEnv = f s.appLogEnv}) m)

instance KatipContext AppM where
  getKatipContext = asks appLogContext
  localKatipContext f (AppM m) = AppM (local (\s -> s {appLogContext = f s.appLogContext}) m)

  getKatipNamespace = asks appLogNamespace
  localKatipNamespace f (AppM m) = AppM (local (\s -> s {appLogNamespace = f s.appLogNamespace}) m)

-- | Generate the connection pool from the connection information
makePostgresPool :: MonadUnliftIO m => ConnectionString -> m (Pool SqlBackend)
makePostgresPool connectionString =
  checkpoint "makePostgresPool" $
    -- TODO: 'runNoLoggingT' is probably not correct here...
    runNoLoggingT $
      createPostgresqlPool connectionString 10

-- | Generate our application environment
makeApp :: Configuration -> IO App
makeApp Configuration {..} = do
  appPostgresPool <- makePostgresPool $ postgresConnectInfoToConnectionString postgresConnectInfo

  -- TODO: Temporary until we decide on a more permanent schema
  liftIO $ runSqlPool (runMigration migrateAll) appPostgresPool

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

-- | Use katip logger in 'IO'
logIO :: App -> Namespace -> Severity -> LogStr -> IO ()
logIO a n s lst = runReaderT (runAppM $ logMsg n s lst) a
