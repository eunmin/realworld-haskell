{-# LANGUAGE DeriveAnyClass #-}

module RealWorld.Infra.Web.Handler.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Relude

data UserWrapper a = UserWrapper
  { user :: a
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance (ToSchema a) => ToSchema (UserWrapper a)

data ProfileWrapper a = ProfileWrapper
  { profile :: a
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance (ToSchema a) => ToSchema (ProfileWrapper a)

data ArticleWrapper a = ArticleWrapper
  { article :: a
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance (ToSchema a) => ToSchema (ArticleWrapper a)

data CommentWrapper a = CommentWrapper
  { comment :: a
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance (ToSchema a) => ToSchema (CommentWrapper a)