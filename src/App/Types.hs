module App.Types where

import Data.Pool (Pool)
import Database.Esqueleto.Experimental
import My.Prelude

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
  , appPostgresPool :: Pool SqlBackend
  }
