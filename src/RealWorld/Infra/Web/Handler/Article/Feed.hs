{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.Feed where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Time (UTCTime)
import RealWorld.Domain.Query.Data (Article, ArticleList, FeedArticlesParams (..))
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.Handler.Types (ArticleWrapper, ProfileWrapper, UserWrapper)
import Relude
import Servant (Capture, Delete, Get, JSON, Post, QueryParam, ReqBody, (:>))

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