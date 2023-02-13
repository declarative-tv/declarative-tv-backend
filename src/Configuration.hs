module Configuration where

import Data.Word (Word16)
import Database.PostgreSQL.Simple (ConnectInfo (..), defaultConnectInfo)
import GHC.Generics (Generic)
import My.Prelude
import System.Envy

newtype Configuration = Configuration
  { postgresConnectInfo :: ConnectInfo
  }

data PostgresConnectInfo = PostgresConnectInfo
  { postgresHost :: String
  , postgresPort :: Word16
  , postgresUser :: String
  , postgresDatabase :: String
  }
  deriving stock (Show, Eq, Generic)

instance DefConfig PostgresConnectInfo where
  defConfig =
    PostgresConnectInfo
      { postgresHost = "localhost"
      , postgresPort = 5432
      , postgresUser = "chiroptical"
      , postgresDatabase = "declarative-tv"
      }

instance FromEnv PostgresConnectInfo where
  fromEnv =
    gFromEnvCustom
      Option
        { dropPrefixCount = 0
        , customPrefix = "DECLARATIVE"
        }

postgresConnectInfoToConnectInfo :: PostgresConnectInfo -> ConnectInfo
postgresConnectInfoToConnectInfo PostgresConnectInfo {..} =
  defaultConnectInfo
    { connectHost = postgresHost
    , connectPort = postgresPort
    , connectUser = postgresUser
    , connectDatabase = postgresDatabase
    }

getConfiguration :: IO Configuration
getConfiguration = do
  postgresConnectInfo <-
    postgresConnectInfoToConnectInfo <$> decodeWithDefaults defConfig
  pure Configuration {..}
