{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Interpreter.Adapter.Repository.ArticleRepository where

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Dynamic (Reader)
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository (..))
import qualified RealWorld.Infra.Component.Database as Database
import qualified RealWorld.Infra.Database.PgArticleRepository as PgArticleRepository
import Relude hiding (Reader)

run :: (Reader Database.State :> es, IOE :> es) => Eff (ArticleRepository : es) a -> Eff es a
run = interpret $ \_ -> \case
  Save article -> PgArticleRepository.save article
  FindBySlug slug -> PgArticleRepository.findBySlug slug
  Delete article -> PgArticleRepository.delete article