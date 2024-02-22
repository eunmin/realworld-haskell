module RealWorld.Domain.Adapter.Repository.CommentRepository where

import Data.ULID (ULID)
import RealWorld.Domain.Command.Article.Entity.Comment
import Relude

class (Monad m) => CommentRepository m where
  save :: Comment -> m ()
  findAllByArticleId :: ULID -> m [Comment]
  delete :: Comment -> m ()