module RealWorld.Domain.Adapter.Repository.FavoriteRepository where

import RealWorld.Domain.Command.Article.Entity.Favorite (Favorite)
import RealWorld.Domain.Command.Article.Value (FavoriteId)
import Relude

class FavoriteRepository m where
  findById :: FavoriteId -> m (Maybe Favorite)
  save :: Favorite -> m Bool
  delete :: Favorite -> m Bool
