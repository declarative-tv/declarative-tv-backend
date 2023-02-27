{-# LANGUAGE ViewPatterns #-}

module Warp.Exception where

import App (App (..))
import Control.Exception (AsyncException (..))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.IO.Exception (IOErrorType (..))
import My.Prelude
import Network.Wai (Request)
import Network.Wai.Handler.Warp (InvalidRequest)
import System.IO.Error (ioeGetErrorType)
import System.TimeManager (TimeoutThread (..))
import UnliftIO.IO (stderr)

isDisplayableException :: SomeException -> Bool
isDisplayableException se
  | Just ThreadKilled <- fromException se = False
  | Just (_ :: InvalidRequest) <- fromException se = False
  | Just (ioeGetErrorType -> et) <- fromException se
  , et == ResourceVanished || et == InvalidArgument =
      False
  | Just TimeoutThread <- fromException se = False
  | otherwise = True

onException :: (MonadReader App m, MonadIO m) => Maybe Request -> SomeException -> m ()
onException mRequest exception = do
  le <- appLogEnv <$> ask
  when (isDisplayableException exception) $
    runKatipContextT le () "..." $
      logMsg "exception:" ErrorS $
        "Exception: " <> lshow exception

-- TODO: Add https://hackage.haskell.org/package/warp-3.3.24/docs/Network-Wai-Handler-Warp.html#v:setOnExceptionResponse
