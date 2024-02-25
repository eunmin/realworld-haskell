module RealWorld.Domain.Command.Article.Value where

import Data.ULID (ULID)
import Relude

----------------------------------------------------------------------------------------------------
-- Slug

newtype Slug = Slug {unSlug :: Text}
  deriving (Show, Eq, Generic)

mkSlug :: Text -> Maybe Slug
mkSlug = Just . Slug

toSlug :: Title -> Slug
toSlug = Slug . unTitle

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

mkDescription :: Text -> Maybe Description
mkDescription = Just . Description

----------------------------------------------------------------------------------------------------
-- Article Body

newtype ArticleBody = ArticleBody {unArticleBody :: Text}
  deriving (Show, Eq, Generic)

mkArticleBody :: Text -> Maybe ArticleBody
mkArticleBody = Just . ArticleBody

----------------------------------------------------------------------------------------------------
-- Comment Body

newtype CommentBody = CommentBody {unCommentBody :: Text}
  deriving (Show, Eq, Generic)

mkCommentBody :: Text -> Maybe CommentBody
mkCommentBody = Just . CommentBody

----------------------------------------------------------------------------------------------------
-- Tag

newtype Tag = Tag {unTag :: Text}
  deriving (Show, Eq, Generic)

mkTag :: Text -> Maybe Tag
mkTag = Just . Tag

----------------------------------------------------------------------------------------------------
-- FavoriteId

data FavoriteId = FavoriteId
  { favoriteIdArticleId :: ULID,
    favoriteIdUserId :: ULID
  }
  deriving (Show, Eq, Generic)

mkFavoriteId :: ULID -> ULID -> FavoriteId
mkFavoriteId = FavoriteId