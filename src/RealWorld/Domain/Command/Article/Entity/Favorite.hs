{-# LANGUAGE NoFieldSelectors #-}

module RealWorld.Domain.Command.Article.Entity.Favorite where

import Data.Time (UTCTime)
import RealWorld.Domain.Command.Article.Value (FavoriteId)
import Relude

data Favorite = Favorite
  { favoriteId :: FavoriteId,
    createdAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

mkFavorite :: FavoriteId -> UTCTime -> Favorite
mkFavorite = Favorite
