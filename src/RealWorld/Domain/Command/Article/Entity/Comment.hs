module RealWorld.Domain.Command.Article.Entity.Comment where

import Data.Time (UTCTime)
import Data.ULID (ULID)
import RealWorld.Domain.Command.Article.Value (CommentBody)

data Comment = Comment
  { commentId :: ULID,
    commentBody :: CommentBody,
    commentCreatedAt :: UTCTime,
    commentUpdatedAt :: Maybe UTCTime,
    commentAuthorId :: ULID,
    commentArticleId :: ULID
  }
  deriving (Show, Eq, Generic)

mkComment :: ULID -> CommentBody -> UTCTime -> ULID -> ULID -> Comment
mkComment commentId body createdAt authorId articleId =
  Comment
    { commentId = commentId,
      commentBody = body,
      commentCreatedAt = createdAt,
      commentUpdatedAt = Nothing,
      commentAuthorId = authorId,
      commentArticleId = articleId
    }

isDeletable :: Comment -> ULID -> Bool
isDeletable comment actorId = actorId /= commentAuthorId comment