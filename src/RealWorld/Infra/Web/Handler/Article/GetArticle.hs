{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.GetArticle where

import Effectful (Eff)
import qualified Effectful as Eff
import RealWorld.Domain.Query.Data (Article, GetArticleParams (..))
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.Handler.Types (ArticleWrapper)
import Relude
import Servant (Capture, Get, JSON, (:>))

type Route =
  "articles"
    :> Capture "slug" Text
    :> Get '[JSON] (ArticleWrapper Article)

handler :: (QueryService Eff.:> es) => ApiAuth -> Text -> Eff es (Maybe Article)
handler (ApiAuth _ _) slug = do
  QueryService.getArticle (GetArticleParams slug)