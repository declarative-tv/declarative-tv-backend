{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- Work in progress trying to get all the way to servant-generic "from scratch"
--
-- Right now, it is more or less a copy of
-- https://github.com/dmjio/servant-from-scratch/blob/master/Main.lhs
module Learn.ServantGeneric where

import Data.Data (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude (Bool (..), String)

-- Bool :<|> Int
data left :<|> right = left :<|> right
infixr 3 :<|>

-- -XPolykinds
-- Bool :> Int
data (a :: k) :> b
infixr 4 :>

-- Q: What is the difference between these two?

-- Emulate Data.Aeson without bringing it in..
data JSON
data Value

data Method
  = GET
  | PUT
  | POST
  | DELETE
  | PATCH

data Capture (name :: Symbol) (captureType :: a)

data QueryParam (name :: Symbol) (queryType :: a)

data RequestBody (ctypes :: [*]) (requestBodyType :: *)

data Verb (method :: Method) (ctypes :: [*]) (returnType :: a)

type Get = Verb 'GET
type Post = Verb 'POST
type Put = Verb 'PUT

type FirstApi = "first" :> Get '[JSON] Value
type SecondApi = "second" :> Get '[JSON] Value
type BothApi = "api" :> FirstApi :<|> SecondApi

-- :kind! Nul '[]
-- :kind! Nul '[1]
type family Nul (xs :: [k]) :: Bool where
  Nul '[] = 'True
  Nul (_ ': _) = 'False

-- :kind! '[1] ++ '[2]
type family a ++ b :: [k] where
  a ++ '[] = a
  '[] ++ b = b
  (x ': xs) ++ a = x ': xs ++ a

-- -XUndecidableInstances
type family Segments (xs :: k) :: [Symbol] where
  Segments (Verb _ _ _) = '[]
  Segments (left :<|> right) = Segments left ++ Segments right
  Segments (symbol :> xs) = symbol ': Segments xs

class ReifySymbols (xs :: [Symbol]) where
  reifySymbols :: Proxy xs -> [String]

instance ReifySymbols '[] where
  reifySymbols _ = []

instance (ReifySymbols xs, KnownSymbol symbol) => ReifySymbols (symbol ': xs) where
  reifySymbols _ = symbolVal (Proxy @symbol) : reifySymbols (Proxy @xs)

segmentsAsValues :: [String]
segmentsAsValues = reifySymbols (Proxy @(Segments BothApi))
