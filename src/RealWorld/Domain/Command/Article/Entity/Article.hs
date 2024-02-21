{-# LANGUAGE StrictData #-}

module RealWorld.Domain.Command.Article.Entity.Article where

import Data.Time (UTCTime)
import Data.ULID (ULID)
import RealWorld.Domain.Command.Article.Value
  ( Body,
    Description,
    Slug,
    Tag,
    Title,
    toSlug,
  )
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

mkArticle :: ULID -> Title -> Description -> Body -> [Tag] -> UTCTime -> ULID -> Article
mkArticle articleId title description body tags createdAt authorId =
  Article
    { articleId = articleId,
      articleSlug = toSlug title,
      articleTitle = title,
      articleDescription = description,
      articleBody = body,
      articleTags = tags,
      articleCreatedAt = createdAt,
      articleUpdatedAt = Nothing,
      articleFavorited = False,
      articleFavoritesCount = 0,
      articleAuthorId = authorId
    }
