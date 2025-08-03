{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.Feed where

import RealWorld.Domain.Query.Data (ArticleList, FeedArticlesParams (..))
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import Relude
import Servant (Get, JSON, QueryParam, (:>))

type Route =
  "articles"
    :> "feed"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] ArticleList

handler ::
  (QueryService m) =>
  ApiAuth ->
  Maybe Int ->
  Maybe Int ->
  m ArticleList
handler (ApiAuth userId _) limit offset = do
  let params =
        FeedArticlesParams
          { actorId = userId
          , limit = fromMaybe 20 limit
          , offset = fromMaybe 0 offset
          }
  QueryService.feedArticles params