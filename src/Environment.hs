module Environment where

import Data.Word (Word16)
import Database.PostgreSQL.Simple (ConnectInfo (..), defaultConnectInfo)
import GHC.Generics (Generic)
import System.Envy

data PgConnectInfo = PgConnectInfo
  { pgHost :: String
  , pgPort :: Word16
  , pgUser :: String
  , pgDatabase :: String
  }
  deriving stock (Show, Eq, Generic)

instance DefConfig PgConnectInfo where
  defConfig =
    PgConnectInfo
      { pgHost = "localhost"
      , pgPort = 5432
      , pgUser = "chiroptical"
      , pgDatabase = "declarative-tv"
      }

instance FromEnv PgConnectInfo where
  fromEnv =
    gFromEnvCustom
      Option
        { dropPrefixCount = 0
        , customPrefix = "DECLARATIVE"
        }

pgConnectInfoToConnectInfo :: PgConnectInfo -> ConnectInfo
pgConnectInfoToConnectInfo PgConnectInfo {..} =
  defaultConnectInfo
    { connectHost = pgHost
    , connectPort = pgPort
    , connectUser = pgUser
    , connectDatabase = pgDatabase
    }

getPgConnectInfo :: IO ConnectInfo
getPgConnectInfo = pgConnectInfoToConnectInfo <$> decodeWithDefaults defConfig
