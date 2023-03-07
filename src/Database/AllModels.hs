{-# LANGUAGE TemplateHaskell #-}

module Database.AllModels (allModelFiles) where

import Data.FileEmbed (embedDirListing)
import My.Prelude

allModelFiles :: [FilePath]
allModelFiles = ("./models/" <>) <$> $(embedDirListing "./models")
