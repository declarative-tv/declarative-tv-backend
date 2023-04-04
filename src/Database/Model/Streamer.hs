{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Model.Streamer where

import Data.UUID (UUID)
import Database.Orphan ()
import Database.Persist.Quasi
import Database.Persist.TH
import Model.StreamingPlatform
import My.Prelude

share
  [mkPersist sqlSettings]
  $(persistFileWith lowerCaseSettings "./models/streamer.persistentmodels")
