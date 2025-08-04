{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Domain.Adapter.Repository.ArticleRepository where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import RealWorld.Domain.Command.Article.Entity.Article (Article)
import RealWorld.Domain.Command.Article.Value (Slug)
import Relude

data ArticleRepository :: Effect where
  Save :: Article -> ArticleRepository m Bool
  FindBySlug :: Slug -> ArticleRepository m (Maybe Article)
  Delete :: Article -> ArticleRepository m Bool

makeEffect ''ArticleRepository
