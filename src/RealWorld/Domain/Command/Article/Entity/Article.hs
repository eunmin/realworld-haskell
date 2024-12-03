{-# LANGUAGE StrictData #-}

module RealWorld.Domain.Command.Article.Entity.Article where

import Data.Time (UTCTime)
import Data.ULID (ULID)
import RealWorld.Domain.Command.Article.Value (
  ArticleBody,
  Description,
  Slug,
  Tag,
  Title,
  toSlug,
 )

----------------------------------------------------------------------------------------------------
-- Article

data Article = Article
  { articleId :: ULID
  , articleSlug :: Slug
  , articleTitle :: Title
  , articleDescription :: Description
  , articleBody :: ArticleBody
  , articleTags :: [Tag]
  , articleCreatedAt :: UTCTime
  , articleUpdatedAt :: Maybe UTCTime
  , articleFavoritesCount :: Int
  , articleAuthorId :: ULID
  }
  deriving stock (Show, Eq, Generic)

mkArticle :: ULID -> Title -> Description -> ArticleBody -> [Tag] -> UTCTime -> ULID -> Article
mkArticle articleId title description body tags createdAt authorId =
  Article
    { articleId = articleId
    , articleSlug = toSlug title
    , articleTitle = title
    , articleDescription = description
    , articleBody = body
    , articleTags = tags
    , articleCreatedAt = createdAt
    , articleUpdatedAt = Nothing
    , articleFavoritesCount = 0
    , articleAuthorId = authorId
    }

update ::
  Article ->
  Maybe Title ->
  Maybe Description ->
  Maybe ArticleBody ->
  Article
update article title description body =
  article
    { articleTitle = title ?: articleTitle article
    , articleDescription = description ?: articleDescription article
    , articleBody = body ?: articleBody article
    , articleSlug = maybe (toSlug $ articleTitle article) toSlug title
    }

isEditable :: Article -> ULID -> Bool
isEditable article actorId = actorId == articleAuthorId article

isDeletable :: Article -> ULID -> Bool
isDeletable article actorId = actorId == articleAuthorId article

increseFavoritesCount :: Article -> Article
increseFavoritesCount article =
  article{articleFavoritesCount = articleFavoritesCount article + 1}

decreseFavoritesCount :: Article -> Article
decreseFavoritesCount article =
  article{articleFavoritesCount = articleFavoritesCount article - 1}