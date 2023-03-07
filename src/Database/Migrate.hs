{-# LANGUAGE TemplateHaskell #-}

module Database.Migrate where

import Database.AllModels
import Database.Model.Streamer
import Database.Persist.Quasi
import Database.Persist.TH

{- TODO: This is temporary while we are working on the application.
 - Eventually, we'll chose and set up a migration framework.
 -}
mkMigrate
  "migrateAll"
  $(persistManyFileWith lowerCaseSettings allModelFiles)
