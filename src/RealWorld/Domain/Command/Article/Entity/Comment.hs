{-# LANGUAGE NoFieldSelectors #-}

module RealWorld.Domain.Command.Article.Entity.Comment where

import Data.Time (UTCTime)
import Data.ULID (ULID)
import RealWorld.Domain.Command.Article.Value (CommentBody)
import Relude

data Comment = Comment
  { commentId :: ULID,
    body :: CommentBody,
    createdAt :: UTCTime,
    updatedAt :: Maybe UTCTime,
    authorId :: ULID,
    articleId :: ULID
  }
  deriving stock (Show, Eq, Generic)

mkComment :: ULID -> CommentBody -> UTCTime -> ULID -> ULID -> Comment
mkComment commentId body createdAt authorId articleId =
  Comment
    { commentId = commentId,
      body = body,
      createdAt = createdAt,
      updatedAt = Nothing,
      authorId = authorId,
      articleId = articleId
    }

isDeletable :: Comment -> ULID -> Bool
isDeletable comment actorId = actorId /= comment.authorId
