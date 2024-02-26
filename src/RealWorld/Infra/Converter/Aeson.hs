{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RealWorld.Infra.Converter.Aeson where

import Data.Aeson
  ( ToJSON (toJSON),
    defaultOptions,
    genericToJSON,
    withText,
  )
import Data.Aeson.Casing (aesonDrop, aesonPrefix, camelCase)
import Data.Aeson.Types (FromJSON (..))
import Data.ULID (ULID)
import RealWorld.Domain.Command.Article.UseCase
  ( AddCommentsError,
    CreateArticleError,
    DeleteArticleError,
    DeleteCommentError,
    FavoriteArticleError,
    UnfavoriteArticleError,
    UpdateArticleError,
  )
import RealWorld.Domain.Command.User.UseCase
  ( AuthenticationError,
    FollowUserError,
    RegistrationError,
    UnfollowUserError,
    UpdateUserError,
  )
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

instance ToJSON RegistrationError where
  toJSON = genericToJSON defaultOptions

instance ToJSON AuthenticationError where
  toJSON = genericToJSON defaultOptions

instance ToJSON UpdateUserError where
  toJSON = genericToJSON defaultOptions

instance ToJSON FollowUserError where
  toJSON = genericToJSON defaultOptions

instance ToJSON UnfollowUserError where
  toJSON = genericToJSON defaultOptions

instance ToJSON CreateArticleError where
  toJSON = genericToJSON defaultOptions

instance ToJSON UpdateArticleError where
  toJSON = genericToJSON defaultOptions

instance ToJSON DeleteArticleError where
  toJSON = genericToJSON defaultOptions

instance ToJSON AddCommentsError where
  toJSON = genericToJSON defaultOptions

instance ToJSON DeleteCommentError where
  toJSON = genericToJSON defaultOptions

instance ToJSON FavoriteArticleError where
  toJSON = genericToJSON defaultOptions

instance ToJSON UnfavoriteArticleError where
  toJSON = genericToJSON defaultOptions

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
