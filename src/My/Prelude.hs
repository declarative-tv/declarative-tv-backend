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
) where

import Colog
import Control.Monad.Catch as MTL (MonadThrow, throwM)
import Control.Monad.Reader as MTL
import Data.Text as Text (Text, pack)
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
