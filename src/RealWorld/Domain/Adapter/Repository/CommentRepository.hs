module RealWorld.Domain.Adapter.Repository.CommentRepository where

import Data.ULID (ULID)
import RealWorld.Domain.Command.Article.Entity.Comment (Comment)

class CommentRepository m where
  save :: Comment -> m Bool
  findById :: ULID -> m (Maybe Comment)
  delete :: Comment -> m Bool