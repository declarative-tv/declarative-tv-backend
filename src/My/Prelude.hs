module My.Prelude (
  -- * Prelude

  -- | Hide partial functions from Haskell's base library
  module Prelude,

  -- * Text
  module Text,
  tshow,

  -- * MTL
  module MTL,

  -- * katip
  module Katip,
  lshow,

  -- * Annotated Exception
  module AnnotatedException,

  -- * Esqueleto

  --

  -- | To use Persistent use,
  --
  -- @ import qualified Database.Persistent as P
  module Esqueleto,

  -- * Time
  module Time,
) where

import Control.Exception.Annotated.UnliftIO as AnnotatedException hiding (Handler)
import Control.Monad.Catch as MTL (MonadThrow, throwM)
import Control.Monad.Reader as MTL
import Data.Text as Text (Text, pack)
import Data.Time.Clock as Time
import Database.Esqueleto.Experimental as Esqueleto
import Katip
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

lshow :: Show a => a -> LogStr
lshow = logStr . show
