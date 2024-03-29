{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- Work in progress trying to get all the way to servant-generic "from scratch"
--
-- Right now, it is more or less a copy of
-- https://github.com/dmjio/servant-from-scratch/blob/master/Main.lhs
module Learn.ServantGeneric where

import Control.Applicative ((<|>))
import Data.Data (Proxy (..))
import Data.Text (pack, splitOn, unpack)
import GHC.Generics
import GHC.TypeLits
import Prelude

-- | Bool :<|> Int
data left :<|> right = left :<|> right

infixr 3 :<|>

{- | Requires '-XPolyKinds', 'a :: k' in this case could be

 - `"path"` which is a 'Symbol' describing a url path
 - `QueryParam "param" SomeType` describing a `?param=x` of the url

 The point is that it must be 'Type'

e.g.
 > :kind! Bool :> Int
-}
data (a :: k) :> b

infixr 4 :>

-- Q: What is the difference between these two?
--
-- We need two type level operators to differentiate between the path builders
-- ('Capture', 'QueryParam', etc) and the APIs. See 'BothApi' below.

-- Emulate Data.Aeson without bringing it in...
data JSON
data Value

data Method
  = GET
  | PUT
  | POST
  | DELETE
  | PATCH

-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/poly_kinds.html#principles-of-kind-inference
data Capture (name :: Symbol) (captureType :: k)

data QueryParam (name :: Symbol) (queryType :: k)

data RequestBody (ctypes :: [a]) (requestBodyType :: k)

data Verb (method :: Method) (ctypes :: [a]) (returnType :: k)

type Get = Verb 'GET
type Post = Verb 'POST
type Put = Verb 'PUT

type FirstApi = "first" :> Capture "first" Int :> Get '[JSON] Value
type SecondApi = "second" :> Get '[JSON] Value
type BothApi = "api" :> (FirstApi :<|> SecondApi)

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
  Segments (Capture symbol _ :> xs) = symbol ': Segments xs

class ReifySymbols (xs :: [Symbol]) where
  reifySymbols :: Proxy xs -> [String]

instance ReifySymbols '[] where
  reifySymbols _ = []

instance (ReifySymbols xs, KnownSymbol symbol) => ReifySymbols (symbol ': xs) where
  reifySymbols _ = symbolVal (Proxy @symbol) : reifySymbols (Proxy @xs)

segmentsAsValues :: [String]
segmentsAsValues = reifySymbols (Proxy @(Segments BothApi))

data Mammal
  = Person
      { name :: String
      , age :: Int
      }
  | Animal
      { name :: String
      , age :: Int
      }
  deriving stock (Eq, Show, Generic)

{- | Links
 (:*:) - https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-Generics.html#t::-42-:
 (:+:) - https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-Generics.html#t::-43-:
 S1 - https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-Generics.html#t:S1
 M1 - https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-Generics.html#t:M1
-}
type family RecordFields (xs :: k) :: [Symbol] where
  RecordFields (a :*: b) = RecordFields a ++ RecordFields b
  RecordFields (a :+: b) = RecordFields a ++ RecordFields b
  RecordFields (S1 ('MetaSel ('Just s) _ _ _) _) = '[s]
  RecordFields (M1 _ _ p) = RecordFields p

-- 'Rep' - https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-Generics.html#t:Rep
mammalFields :: [String]
mammalFields = reifySymbols (Proxy @(RecordFields (Rep Mammal)))

-- What is 'M1' and 'S1'?

{- | :kind! Rep M1Example

 Will yield `M1 D _meta (M1 S _meta (K1 R Int))`

 The 'M1' just means meta-information
 The 'D' means we have a data type
 The 'S' means we have a record selector
 The 'K1' means constants, additional parameters and recursion of kind '*'
-}
newtype M1Example = M1Example {unM1Example :: Int}
  deriving stock (Generic)

{- | Try removing the 'unM1Example' field from 'M1Example', what do you think
 will happen?

 We'll never find an 'S1' with appropriate 'Meta' information
 i.e. a record selector
-}
m1ExampleFields :: [String]
m1ExampleFields = reifySymbols (Proxy @(RecordFields (Rep M1Example)))

type family Server a
type instance Server (left :<|> right) = Server left :<|> Server right

{- | In servant, this would if there are no more captures, i.e. 'xs :: '[]', we
 would describe how to encode the result 'a'.
-}
type instance Server (Get xs a) = IO Response

type instance Server ((s :: Symbol) :> r) = Server r

{- | In servant, the right side would be 'a -> Server r' because we would try
 to parse the capture as 'a' before continuing.
-}
type instance Server (Capture (s :: Symbol) a :> r) = String -> Server r

-- None of this really matters because our server doesn't do anything...
type Request = String
type Response = String
type ResponseReceived = String

class HasServer a where
  route ::
    Proxy a ->
    Server a ->
    [String] -> -- path
    Request -> -- request
    (Response -> IO ResponseReceived) -> -- response
    Maybe (IO ResponseReceived)

{- | The ':<|>' is a branch, we can either follow the left or the right.
 Be careful with parentheses, e.g.
 @
  type BothApi = "api" :> FirstApi :<|> SecondApi
 @

 is different than,

 @
  type BothApi = "api" :> (FirstApi :<|> SecondApi)
 @

 The fixity means the first is implicitly

 @
  type BothApi = ("api" :> FirstApi) :<|> SecondApi
 @
-}
instance (HasServer left, HasServer right) => HasServer (left :<|> right) where
  route _ (l :<|> r) path req resp =
    route (Proxy @left) l path req resp <|> route (Proxy @right) r path req resp

{- | Capture the path and send it to the handler, additionally move to the next
 part of the path. We aren't done until we find a 'Verb'.

 In servant, we would try to interpret the 'KnownSymbol' `name` as type `typ`
 Here, we don't do that for simplicity. If it can't interpret the type, we
 would look at other routes that might match.

 'QueryParam' is handled very similarly
-}
instance
  (KnownSymbol name, HasServer right) =>
  HasServer (Capture name typ :> right)
  where
  route _ server (l : ls) req resp =
    route (Proxy @right) (server l) ls req resp
  route _ _ _ _ _ = Nothing

{- | Compare the current path to the server's path. If they are equal,
 continue, else we are done. We are basically walking the path together. If
 that isn't possible, we are following a different route's path.
-}
instance (KnownSymbol s, HasServer r) => HasServer (s :> r) where
  route _ server (l : ls) req resp
    | l == symbolVal (Proxy @s) =
        route (Proxy @r) server ls req resp
  route _ _ _ _ _ = Nothing

{- | A termination condition if the path has been fully traverse do the thing.
 In this case, we pass the server result to the response function. In the
 examples below, that is just 'pure' for simplicity.
-}
instance HasServer (Get xs a) where
  route _ server [] _ resp = Just $ server >>= resp
  route _ _ _ _ _ = Nothing

-- | In servant, this would return 'IO a'
type Application = Request -> (Response -> IO ResponseReceived) -> IO String

-- | Serve the application, complain if we can't find a valid path
serve :: HasServer a => Proxy a -> Server a -> Application
serve proxy handler req resp =
  case route proxy handler paths req resp of
    Nothing -> resp "invalid path..."
    Just result -> result
  where
    paths = unpack <$> splitOn "/" (pack req)

{- | In the original version of servant we had to combine handlers with :<|>.
 The type errors are pretty bad using this style.
-}
runServer :: String -> IO String
runServer path = serve (Proxy @BothApi) bothHandler path pure
  where
    bothHandler :: Server BothApi
    bothHandler = firstHandler :<|> secondHandler
    firstHandler :: Response -> IO ResponseReceived
    firstHandler = pure
    secondHandler :: IO ResponseReceived
    secondHandler = pure "second handler"

-- This is essentially how the original servant works
--
-- Let's now look at servant-generic's code

{- | Allows us to dress up a record in different styles. We can re-use the same
 record type to interpret different modes. For example, what type should the
 Api have? What type should the Server have?
-}
type family mode :- api

infixl 0 :-

data AsApi
type instance AsApi :- api = api

data AsServer
type instance AsServer :- api = Server api

-- Could also have AsClient, AsServerT, AsClientT, etc...

{- | Everything to the right of ':-' is the same as before, but we can
 interpret the 'Example AsApi' or 'Example AsServer' to get different types.
 This is called "higher-kinded data" or HKD.
-}
data Example route = Example
  { first :: route :- "first" :> Capture "this" String :> Get '[JSON] Value
  , second :: route :- "second" :> Get '[JSON] Value
  }
  deriving stock (Generic)

{- | An associated type class that converts the representation of a record type
 in Generic to Servant's ':<|>'. Essentially, each field in the record is a
 route!
-}
class GProduct f where
  type GToServant f
  gToServant :: f p -> GToServant f
  gFromServant :: GToServant f -> f p

instance (GProduct left, GProduct right) => GProduct (left :*: right) where
  type GToServant (left :*: right) = GToServant left :<|> GToServant right
  gToServant (left :*: right) = gToServant left :<|> gToServant right
  gFromServant (left :<|> right) = gFromServant left :*: gFromServant right

instance GProduct f => GProduct (M1 i c f) where
  type GToServant (M1 i c f) = GToServant f -- a record selector, more recursion
  gToServant (M1 x) = gToServant x
  gFromServant = M1 . gFromServant

instance GProduct (K1 i c) where
  type GToServant (K1 i c) = c -- The route's type
  gToServant (K1 x) = x
  gFromServant = K1

{- |
 @
   > :kind! ToServant (Example AsApi)
 @

 This is similar to 'BothApi' from above

 @
   > :kind! ToServant (Example AsServer)
 @

 This is the server's type
-}
type ToServant a = GToServant (Rep a)

{- | -XConstraintKinds
 A constraint kind where 'a' is 'Generic' and a 'GProduct'
-}
type GenericProduct a = (Generic a, GProduct (Rep a))

{- | e.g.

 @
   > :type toServant exampleImpl
   SG.toServant SG.exampleImpl
    :: (String -> IO SG.Response) SG.:<|> IO SG.Response
 @

 This defines the shape of the server
-}
toServant :: GenericProduct a => a -> ToServant a
toServant = gToServant . from

-- | I don't know how this is used within servant...
fromServant :: GenericProduct a => ToServant a -> a
fromServant = to . gFromServant

{- | An implementation of our server. The type inference here is a lot better
 than before. Literally my favorite part of using servant's generic features.
-}
exampleImpl :: Example AsServer
exampleImpl =
  Example
    { first = pure :: String -> IO String
    , second = pure "second" :: IO String
    }

type ToServantApi routes = ToServant (routes AsApi)
type ToServantServer routes = ToServant (routes AsServer)

genericServe ::
  forall routes.
  ( -- the API type must be an instance of 'HasServer'
    HasServer (ToServantApi routes)
  , -- the constraints that enable the Generic machinery
    GenericProduct (routes AsServer)
  , -- remind GHC that these are the same. The former is for
    -- 'serve' the latter is for `toServant`.
    --
    -- ghci> :kind! Server (ToServantApi Example)
    -- Server (ToServantApi Example) :: *
    -- = ([Char] -> IO [Char]) :<|> IO [Char]
    --
    -- ghci> :kind! ToServant (Example AsServer)
    -- ToServant (Example AsServer) :: *
    -- = ([Char] -> IO [Char]) :<|> IO [Char]
    Server (ToServantApi routes) ~ ToServant (routes AsServer)
  ) =>
  routes AsServer ->
  Application
genericServe = serve (Proxy @(ToServantApi routes)) . toServant

-- | Look mom, no handlers!
genericRunServer :: String -> IO Response
genericRunServer path = genericServe exampleImpl path pure
