module RealWorld.Domain.Adapter.Repository.ArticleRepository where

import Data.ULID (ULID)
import RealWorld.Domain.Command.Article.Entity.Article (Article)
import RealWorld.Domain.Command.Article.Value (Slug)
import Relude

class ArticleRepository m where
  save :: Article -> m Bool
  findById :: ULID -> m (Maybe Article)
  findBySlug :: Slug -> m (Maybe Article)
  delete :: Article -> m Bool