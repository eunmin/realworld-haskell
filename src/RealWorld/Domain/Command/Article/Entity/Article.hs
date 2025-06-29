{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module RealWorld.Domain.Command.Article.Entity.Article where

import Data.Time (UTCTime)
import Data.ULID (ULID)
import RealWorld.Domain.Command.Article.Value
  ( ArticleBody,
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
    slug :: Slug,
    title :: Title,
    description :: Description,
    body :: ArticleBody,
    tags :: [Tag],
    createdAt :: UTCTime,
    updatedAt :: Maybe UTCTime,
    favoritesCount :: Int,
    authorId :: ULID
  }
  deriving stock (Show, Eq, Generic)

mkArticle :: ULID -> Title -> Description -> ArticleBody -> [Tag] -> UTCTime -> ULID -> Article
mkArticle articleId title description body tags createdAt authorId =
  Article
    { articleId = articleId,
      slug = toSlug title,
      title = title,
      description = description,
      body = body,
      tags = tags,
      createdAt = createdAt,
      updatedAt = Nothing,
      favoritesCount = 0,
      authorId = authorId
    }

update ::
  Article ->
  Maybe Title ->
  Maybe Description ->
  Maybe ArticleBody ->
  Article
update article title description body =
  article
    { title = title ?: article.title,
      description = description ?: article.description,
      body = body ?: article.body,
      slug = maybe (toSlug $ article.title) toSlug title
    }

isEditable :: Article -> ULID -> Bool
isEditable article actorId = actorId == article.authorId

isDeletable :: Article -> ULID -> Bool
isDeletable article actorId = actorId == article.authorId

increseFavoritesCount :: Article -> Article
increseFavoritesCount article =
  article {favoritesCount = article.favoritesCount + 1}

decreseFavoritesCount :: Article -> Article
decreseFavoritesCount article =
  article {favoritesCount = article.favoritesCount - 1}
