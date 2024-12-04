module RealWorld.Domain.Command.Article.Value where

import Data.Text (replace, toLower)
import Data.ULID (ULID)

----------------------------------------------------------------------------------------------------
-- Slug

newtype Slug = Slug {unSlug :: Text}
  deriving stock (Show, Eq, Generic)

mkSlug :: Text -> Maybe Slug
mkSlug = Just . Slug

-- TODO: use a better slugify algorithm
toSlug :: Title -> Slug
toSlug = Slug . replace " " "-" . toLower . unTitle

----------------------------------------------------------------------------------------------------
-- Title

newtype Title = Title {unTitle :: Text}
  deriving stock (Show, Eq, Generic)

-- TODO: validate title
mkTitle :: Text -> Maybe Title
mkTitle = Just . Title

----------------------------------------------------------------------------------------------------
-- Description

newtype Description = Description {unDescription :: Text}
  deriving stock (Show, Eq, Generic)

mkDescription :: Text -> Maybe Description
mkDescription = Just . Description

----------------------------------------------------------------------------------------------------
-- Article Body

newtype ArticleBody = ArticleBody {unArticleBody :: Text}
  deriving stock (Show, Eq, Generic)

mkArticleBody :: Text -> Maybe ArticleBody
mkArticleBody = Just . ArticleBody

----------------------------------------------------------------------------------------------------
-- Comment Body

newtype CommentBody = CommentBody {unCommentBody :: Text}
  deriving stock (Show, Eq, Generic)

mkCommentBody :: Text -> Maybe CommentBody
mkCommentBody = Just . CommentBody

----------------------------------------------------------------------------------------------------
-- Tag

newtype Tag = Tag {unTag :: Text}
  deriving stock (Show, Eq, Generic)

mkTag :: Text -> Maybe Tag
mkTag = Just . Tag

----------------------------------------------------------------------------------------------------
-- FavoriteId

data FavoriteId = FavoriteId
  { articleId :: ULID
  , userId :: ULID
  }
  deriving stock (Show, Eq, Generic)

mkFavoriteId :: ULID -> ULID -> FavoriteId
mkFavoriteId = FavoriteId
