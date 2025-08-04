{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Domain.Adapter.Repository.FavoriteRepository where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import RealWorld.Domain.Command.Article.Entity.Favorite (Favorite)
import RealWorld.Domain.Command.Article.Value (FavoriteId)
import Relude

data FavoriteRepository :: Effect where
  FindById :: FavoriteId -> FavoriteRepository m (Maybe Favorite)
  Save :: Favorite -> FavoriteRepository m Bool
  Delete :: Favorite -> FavoriteRepository m Bool

makeEffect ''FavoriteRepository
