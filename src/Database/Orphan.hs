{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Orphan where

import Data.ByteString.Char8 qualified as B8
import Data.Text qualified as Text
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import My.Prelude
import Text.Read (readMaybe)
import Web.PathPieces

instance PersistField UUID where
  fromPersistValue (PersistLiteralEscaped uuidB8) =
    case UUID.fromString $ B8.unpack uuidB8 of
      Just uuid -> Right uuid
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"
  toPersistValue = PersistLiteralEscaped . B8.pack . UUID.toString

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  toPathPiece = tshow
  fromPathPiece = readMaybe . Text.unpack
