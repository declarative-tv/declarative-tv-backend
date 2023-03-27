module Configuration where

import Data.ByteString (ByteString)
import Database.Persist.Postgresql (ConnectionString)
import GHC.Generics (Generic)
import My.Prelude
import System.Envy

newtype Configuration = Configuration
  { postgresConnectInfo :: PostgresConnectInfo
  }

-- | TODO: May want to add password here
data PostgresConnectInfo = PostgresConnectInfo
  { postgresHost :: ByteString
  , postgresPort :: ByteString
  , postgresUser :: ByteString
  , postgresDatabaseName :: ByteString
  }
  deriving stock (Show, Eq, Generic)

instance DefConfig PostgresConnectInfo where
  defConfig =
    PostgresConnectInfo
      { postgresHost = "localhost"
      , postgresPort = "5432"
      , postgresUser = "chiroptical"
      , postgresDatabaseName = "declarative-tv"
      }

instance FromEnv PostgresConnectInfo where
  fromEnv =
    gFromEnvCustom
      Option
        { dropPrefixCount = 0
        , customPrefix = "DECLARATIVE"
        }

getConfiguration :: IO Configuration
getConfiguration = do
  postgresConnectInfo@PostgresConnectInfo {} <- decodeWithDefaults defConfig
  pure $ Configuration {..}

postgresConnectInfoToConnectionString :: PostgresConnectInfo -> ConnectionString
postgresConnectInfoToConnectionString PostgresConnectInfo {..} =
  "host="
    <> postgresHost
    <> " port="
    <> postgresPort
    <> " user="
    <> postgresUser
    <> " dbname="
    <> postgresDatabaseName

-- <> " password=" <> postgresPort
