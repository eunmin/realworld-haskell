{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Interpreter.Adapter.Repository.QueryService where

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Dynamic (Reader)
import RealWorld.Domain.Query.QueryService (QueryService (..))
import qualified RealWorld.Infra.Component.Database as Database
import qualified RealWorld.Infra.Database.PgQueryService as PgQueryService
import Relude hiding (Reader)

run :: (Reader Database.State :> es, IOE :> es) => Eff (QueryService : es) a -> Eff es a
run = interpret $ \_ -> \case
  GetCurrentUser params -> PgQueryService.getCurrentUser params
  GetProfile params -> PgQueryService.getProfile params
  ListArticles params -> PgQueryService.listArticles params
  FeedArticles params -> PgQueryService.feedArticles params
  GetArticle params -> PgQueryService.getArticle params
  GetComments params -> PgQueryService.getComments params
  GetTags -> PgQueryService.getTags