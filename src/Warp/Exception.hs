{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Warp.Exception where

import App (AppM)
import Control.Exception (AsyncException (..))
import GHC.IO.Exception (IOErrorType (..))
import My.Prelude
import Network.HTTP.Types
import Network.Wai (Request, Response, responseLBS)
import Network.Wai.Handler.Warp (InvalidRequest (..))
import System.IO.Error (ioeGetErrorType)
import System.TimeManager (TimeoutThread (..))

isDisplayableException :: SomeException -> Bool
isDisplayableException se
  | Just ThreadKilled <- fromException se = False
  | Just (_ :: InvalidRequest) <- fromException se = False
  | Just (ioeGetErrorType -> et) <- fromException se
  , et == ResourceVanished || et == InvalidArgument =
      False
  | Just TimeoutThread <- fromException se = False
  | otherwise = True

onException :: Maybe Request -> SomeException -> AppM ()
onException mRequest exception =
  when (isDisplayableException exception) $
    logMsg "exception:" ErrorS $
      "Exception: "
        <> lshow exception
        <> "Request: "
        <> lshow mRequest

{- | Currently the default implementation, but may customize in the future.

 - Sending 413 for too large payload.
 - Sending 431 for too large headers.
 - Sending 400 for bad requests.
 - Sending 500 for internal server errors.
-}
onExceptionResponse :: SomeException -> Response
onExceptionResponse e
  | Just PayloadTooLarge <-
      fromException e =
      responseLBS
        status413
        [(hContentType, "text/plain; charset=utf-8")]
        "Payload too large"
  | Just RequestHeaderFieldsTooLarge <-
      fromException e =
      responseLBS
        status431
        [(hContentType, "text/plain; charset=utf-8")]
        "Request header fields too large"
  | Just (_ :: InvalidRequest) <-
      fromException e =
      responseLBS
        badRequest400
        [(hContentType, "text/plain; charset=utf-8")]
        "Bad Request"
  | otherwise =
      responseLBS
        internalServerError500
        [(hContentType, "text/plain; charset=utf-8")]
        "Something went wrong"
