{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RealWorld.Infra.Converter.Aeson where

import Data.Aeson
  ( ToJSON (toJSON),
    genericToJSON,
    withText,
  )
import Data.Aeson.Casing (aesonDrop, aesonPrefix, camelCase)
import Data.Aeson.Types (FromJSON (..))
import Data.ULID (ULID)
import RealWorld.Domain.Command.User.Value
  ( Bio (..),
    Email (..),
    Image (..),
    Token (..),
    Username (..),
  )
import RealWorld.Domain.Query.Data (Article, ArticleList, Comment, CommentList, Profile, TagList, User)
import RealWorld.Domain.Util.BoundedText (BoundedText (..))
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

instance ToJSON Comment where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance ToJSON ArticleList where
  toJSON = genericToJSON $ aesonDrop 11 camelCase

instance ToJSON CommentList where
  toJSON = genericToJSON $ aesonDrop 11 camelCase

instance ToJSON TagList where
  toJSON = genericToJSON $ aesonDrop 7 camelCase
