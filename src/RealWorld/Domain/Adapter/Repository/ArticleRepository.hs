module RealWorld.Domain.Adapter.Repository.ArticleRepository where

import Data.ULID (ULID)
import RealWorld.Domain.Command.Article.Entity.Article (Article)
import Relude

class ArticleRepository m where
  save :: Article -> m ()
  findById :: ULID -> m (Maybe Article)