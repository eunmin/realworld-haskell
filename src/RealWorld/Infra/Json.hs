{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RealWorld.Infra.Json where

import Data.Aeson
  ( ToJSON (toJSON),
    genericToJSON,
    withText,
  )
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.Types (FromJSON (..))
import Data.ULID (ULID)
import RealWorld.Domain.Command.User.Value
  ( Bio (..),
    Email (..),
    Image (..),
    Token (..),
    Username (..),
  )
import RealWorld.Domain.Query.Data (Article, Profile, User)
import RealWorld.Util.BoundedText (BoundedText (..))
import Relude

instance ToJSON ULID where
  toJSON = show

instance FromJSON ULID where
  parseJSON =
    withText "ULID"
      $ maybe (fail "invalid ULID") pure
      . readMaybe
      . toString

deriving newtype instance ToJSON Email

deriving newtype instance ToJSON Bio

deriving newtype instance ToJSON Image

deriving newtype instance ToJSON Token

deriving newtype instance ToJSON (BoundedText min max)

deriving newtype instance ToJSON Username

instance ToJSON User where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance ToJSON Profile where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance ToJSON Article where
  toJSON = genericToJSON $ aesonPrefix camelCase