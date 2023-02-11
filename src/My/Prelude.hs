module My.Prelude (
  -- * Prelude

  -- | Hide partial functions from Haskell's base library
  module Prelude,

  -- * Text
  module Text,

  -- * MTL
 module MTL,
) where

import Control.Monad.Catch as MTL (MonadThrow, throwM)
import Control.Monad.Reader as MTL
import Data.Text as Text (Text)
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
  maximum,
  minimum,
  product,
  reverse,
  sum,
  tail,
  (!!),
 )
