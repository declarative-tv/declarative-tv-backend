{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Model.Streamer where

import Database.Persist.Quasi
import Database.Persist.TH
import My.Prelude

share
  [mkPersist sqlSettings]
  $(persistFileWith lowerCaseSettings "./models/streamer.persistentmodels")
