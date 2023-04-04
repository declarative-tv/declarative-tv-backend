{-# LANGUAGE TemplateHaskell #-}

module Model.StreamingPlatform where

import Database.Persist.TH
import My.Prelude

data StreamingPlatform
  = Twitch
  | Youtube
  deriving stock (Show, Read)

derivePersistField "StreamingPlatform"
