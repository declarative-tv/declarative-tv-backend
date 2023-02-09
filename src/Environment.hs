{-# OPTIONS_GHC -Wno-unused-imports #-}

module Environment where

import Database.PostgreSQL.Simple (ConnectInfo)
import Environment.Orphans
import System.Envy

getPgConnectInfo :: IO ConnectInfo
getPgConnectInfo = decodeWithDefaults defConfig
