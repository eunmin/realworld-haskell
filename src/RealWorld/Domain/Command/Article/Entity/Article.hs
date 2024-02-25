{-# LANGUAGE StrictData #-}

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
    articleSlug :: Slug,
    articleTitle :: Title,
    articleDescription :: Description,
    articleBody :: ArticleBody,
    articleTags :: [Tag],
    articleCreatedAt :: UTCTime,
    articleUpdatedAt :: Maybe UTCTime,
    articleFavorited :: Bool,
    articleFavoritesCount :: Int,
    articleAuthorId :: ULID
  }
  deriving (Show, Eq, Generic)

mkArticle :: ULID -> Title -> Description -> ArticleBody -> [Tag] -> UTCTime -> ULID -> Article
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

update ::
  Article ->
  ULID ->
  Maybe Title ->
  Maybe Description ->
  Maybe ArticleBody ->
  Maybe Article
update article actorId title description body =
  if actorId /= articleAuthorId article
    then Nothing
    else
      Just
        $ article
          { articleTitle = title ?: articleTitle article,
            articleDescription = description ?: articleDescription article,
            articleBody = body ?: articleBody article,
            articleSlug = maybe (toSlug $ articleTitle article) toSlug title
          }

isDeletable :: Article -> ULID -> Bool
isDeletable article actorId = actorId /= articleAuthorId article

increseFavoritesCount :: Article -> Article
increseFavoritesCount article =
  article {articleFavoritesCount = articleFavoritesCount article + 1}

decreseFavoritesCount :: Article -> Article
decreseFavoritesCount article =
  article {articleFavoritesCount = articleFavoritesCount article - 1}