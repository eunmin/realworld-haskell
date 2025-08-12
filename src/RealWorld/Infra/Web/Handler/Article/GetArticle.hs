{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.GetArticle where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Effectful (Eff)
import qualified Effectful as Eff
import RealWorld.Domain.Query.Data (Article, GetArticleParams (..))
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (Capture, Get, JSON, (:>))

type Route =
  "articles"
    :> Capture "slug" Text
    :> Get '[JSON] GetArticleRequest

data GetArticleRequest = GetArticleRequest
  { article :: Maybe Article
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

handler :: (QueryService Eff.:> es) => Text -> Eff es GetArticleRequest
handler slug = do
  article <- QueryService.getArticle (GetArticleParams slug)
  pure $ GetArticleRequest article