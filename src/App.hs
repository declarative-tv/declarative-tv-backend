{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
-- strictness annotation flags this

module App where

import My.Prelude

-- | The application configuration
data App m = App
  { appLogAction :: !(LogAction m Message)
  }

instance HasLog (App m) Message m where
  getLogAction :: App m -> LogAction m Message
  getLogAction = appLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> App m -> App m
  setLogAction newLogAction app = app {appLogAction = newLogAction}
  {-# INLINE setLogAction #-}

-- | The application environment
newtype AppM a = AppM {runAppM :: ReaderT (App AppM) IO a}
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader (App AppM)
    , MonadThrow
    )
