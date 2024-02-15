{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Conduit.Infra.Json where

import Conduit.Domain.User.Entity (AuthorizedUser, Email (..), Token (..), UserName (..))
import Conduit.Util.BoundedText (BoundedText (..))
import Data.Aeson (Options (unwrapUnaryRecords), ToJSON (toJSON), defaultOptions, genericToJSON, withText)
import Data.Aeson.Casing (aesonDrop, camelCase)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (FromJSON (..))
import Data.ULID (ULID)
import Relude

instance ToJSON ULID where
  toJSON = show

instance FromJSON ULID where
  parseJSON =
    withText "ULID"
      $ maybe (fail "invalid ULID") pure
      . readMaybe
      . toString

$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) 'BoundedText)

$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) 'UserName)

$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) 'Email)

$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) 'Token)

instance ToJSON AuthorizedUser where
  toJSON = genericToJSON $ aesonDrop 14 camelCase