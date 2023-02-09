{-# OPTIONS_GHC -Wno-orphans #-}

module Environment.Orphans where

import Database.PostgreSQL.Simple (ConnectInfo (..), defaultConnectInfo)
import System.Envy

instance DefConfig ConnectInfo where
  defConfig =
    defaultConnectInfo
      { connectHost = "localhost"
      , connectPort = 5432
      , connectUser = "chiroptical"
      , connectDatabase = "declarative-tv"
      }

instance FromEnv ConnectInfo where
  fromEnv =
    gFromEnvCustom
      Option
        { dropPrefixCount = 0
        , customPrefix = "DECLARATIVE"
        }
