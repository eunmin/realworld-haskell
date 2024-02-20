{-# LANGUAGE StrictData #-}

module RealWorld.Domain.Command.Article.Entity.Article where

import Data.Time (UTCTime)
import Data.ULID (ULID)
import Relude

----------------------------------------------------------------------------------------------------
-- Article

data Article = Article
  { articleId :: ULID,
    articleSlug :: Slug,
    articleTitle :: Title,
    articleDescription :: Description,
    articleBody :: Body,
    articleTags :: [Tag],
    articleCreatedAt :: UTCTime,
    articleUpdatedAt :: Maybe UTCTime,
    articleFavorited :: Bool,
    articleFavoritesCount :: Int,
    articleAuthorId :: ULID
  }
  deriving (Show, Eq, Generic)

----------------------------------------------------------------------------------------------------
-- Slug

newtype Slug = Slug {unSlug :: Text}
  deriving (Show, Eq, Generic)

----------------------------------------------------------------------------------------------------
-- Title

newtype Title = Title {unTitle :: Text}
  deriving (Show, Eq, Generic)

mkTitle :: Text -> Maybe Title
mkTitle = Just . Title

----------------------------------------------------------------------------------------------------
-- Description

newtype Description = Description {unDescription :: Text}
  deriving (Show, Eq, Generic)

----------------------------------------------------------------------------------------------------
-- Body

newtype Body = Body {unBody :: Text}
  deriving (Show, Eq, Generic)

----------------------------------------------------------------------------------------------------
-- Tag

newtype Tag = Tag {unTag :: Text}
  deriving (Show, Eq, Generic)
