{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.ListArticle where

import RealWorld.Domain.Query.Data (ArticleList, ListArticlesParams (..))
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import Relude
import Servant (Get, JSON, QueryParam, (:>))

type Route =
  "articles"
    :> QueryParam "tag" Text
    :> QueryParam "author" Text
    :> QueryParam "favorited" Text
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] ArticleList

handler ::
  (QueryService m) =>
  ApiAuth ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  m ArticleList
handler (ApiAuth userId _) tag author favorited limit offset = do
  let params =
        ListArticlesParams
          { actorId = pure userId
          , tag = tag
          , author = author
          , favorited = favorited
          , limit = Just $ fromMaybe 20 limit
          , offset = Just $ fromMaybe 0 offset
          }
  QueryService.listArticles params