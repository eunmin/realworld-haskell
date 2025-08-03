{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.DeleteArticle where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Swagger (ToSchema)
import Data.Time (UTCTime)
import Katip
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository)
import RealWorld.Domain.Adapter.Repository.FavoriteRepository (FavoriteRepository)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.Article.UseCase (CreateArticleCommand (..), CreateArticleError (..), DeleteArticleError (..), UpdateArticleCommand (..), UpdateArticleError (..))
import qualified RealWorld.Domain.Command.Article.UseCase as ArticleUseCase
import RealWorld.Domain.Query.Data (Article (..), GetProfileParams (..), Profile)
import qualified RealWorld.Domain.Query.Data as Query
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.ErrorResponse (badRequest, notFound')
import RealWorld.Infra.Web.Handler.Types (ArticleWrapper, ProfileWrapper, UserWrapper)
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (Capture, Delete, Get, JSON, NoContent (..), Post, Put, ReqBody, ServerError, (:>))

type Route =
  "articles"
    :> Capture "slug" Text
    :> Delete '[JSON] NoContent

instance ToJSON DeleteArticleError

toError :: ArticleUseCase.DeleteArticleError -> ServerError
toError DeleteArticleErrorInvalidUserId = badRequest "Invalid User Id"
toError DeleteArticleErrorInvalidSlug = badRequest "Invalid slug"
toError DeleteArticleErrorArticleNotFound = notFound' "Article not found"
toError DeleteArticleErrorDeletePermissionDenied = badRequest "Delete permission denied"

handler ::
  (KatipContext m, ArticleRepository m, TxManager m, MonadError ServerError m) =>
  ApiAuth ->
  Text ->
  m NoContent
handler (ApiAuth userId _) slug = do
  result <- ArticleUseCase.deleteArticle toCommand
  case result of
    Right _ -> pure NoContent
    Left err -> do
      katipAddContext (sl "error" err) $ do
        $(logTM) ErrorS "deleteArticle error"
      throwError $ toError err
 where
  toCommand :: ArticleUseCase.DeleteArticleCommand
  toCommand =
    ArticleUseCase.DeleteArticleCommand
      { slug = slug
      , userId = userId
      }