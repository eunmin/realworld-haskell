module RealWorld.Domain.Adapter.Repository.CommentRepository where

import Data.ULID (ULID)
import RealWorld.Domain.Command.Article.Entity.Comment (Comment)
import Relude

class (Monad m) => CommentRepository m where
  save :: Comment -> m ()
  findById :: ULID -> m (Maybe Comment)
  delete :: Comment -> m ()