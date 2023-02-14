module My.Prelude (
  -- * Prelude

  -- | Hide partial functions from Haskell's base library
  module Prelude,

  -- * Text
  module Text,
  tshow,

  -- * MTL
  module MTL,

  -- * Co-log
  module Colog,
  logShow,

  -- * Annotated Exception
  module AnnotatedException,
) where

import Colog
import Control.Exception.Annotated.UnliftIO as AnnotatedException hiding (Handler)
import Control.Monad.Catch as MTL (MonadThrow, throwM)
import Control.Monad.Reader as MTL
import Data.Text as Text (Text, pack)
import GHC.Stack (callStack)
import Prelude hiding (
  cycle,
  dropWhile,
  error,
  filter,
  foldl,
  foldl1,
  foldr1,
  head,
  init,
  last,
  length,
  log,
  maximum,
  minimum,
  product,
  reverse,
  sum,
  tail,
  (!!),
 )

tshow :: Show a => a -> Text
tshow = pack . show

{- | Take a 'Show'-able and convert it into co-log formatted 'Text'. Useful
 when you want to log in 'IO' directly.
-}
logShow :: Show a => Severity -> a -> Text
logShow sev = fmtMessage . Msg sev callStack . tshow
