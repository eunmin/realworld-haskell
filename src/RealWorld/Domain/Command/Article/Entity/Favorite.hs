{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module RealWorld.Domain.Command.Article.Entity.Favorite where

import Data.Time (UTCTime)
import RealWorld.Domain.Command.Article.Value (FavoriteId)
import Relude

data Favorite = Favorite
  { favoriteId :: FavoriteId,
    favroiteCreatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

mkFavorite :: FavoriteId -> UTCTime -> Favorite
mkFavorite = Favorite