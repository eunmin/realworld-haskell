{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.UpdateArticle where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Swagger (ToSchema)
import Katip
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository)
import RealWorld.Domain.Adapter.Repository.FavoriteRepository (FavoriteRepository)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.Article.UseCase (UpdateArticleCommand (..), UpdateArticleError (..))
import qualified RealWorld.Domain.Command.Article.UseCase as ArticleUseCase
import RealWorld.Domain.Query.Data (Article (..), Profile)
import qualified RealWorld.Domain.Query.Data as Query
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.ErrorResponse (badRequest, notFound')
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (Capture, JSON, Put, ReqBody, ServerError, (:>))

type Route =
  "articles"
    :> Capture "slug" Text
    :> ReqBody '[JSON] UpdateArticleRequest
    :> Put '[JSON] UpdateArticleResponse

data UpdateArticleRequest = UpdateArticleRequest
  { article :: UpdateArticleInput
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data UpdateArticleInput = UpdateArticleInput
  { title :: Maybe Text
  , description :: Maybe Text
  , body :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data UpdateArticleResponse = UpdateArticleResponse
  { article :: Article
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance ToJSON UpdateArticleError

toError :: ArticleUseCase.UpdateArticleError -> ServerError
toError UpdateArticleErrorInvalidTitle = badRequest "Invalid title"
toError UpdateArticleErrorUserId = badRequest "Invalid user id"
toError UpdateArticleErrorInvalidSlug = badRequest "Invalid slug"
toError UpdateArticleErrorArticleNotFound = notFound' "Article not found"
toError UpdateArticleErrorInvalidBody = badRequest "Invalid body"
toError UpdateArticleErrorInvalidDescription = badRequest "Invalid description"
toError UpdateArticleErrorAuthorNotFound = notFound' "Author not found"
toError UpdateArticleErrorEditPermissionDenied = badRequest "Edit permission denied"

handler ::
  ( KatipContext m
  , ArticleRepository m
  , UserRepository m
  , FavoriteRepository m
  , TxManager m
  , QueryService m
  , MonadError ServerError m
  ) =>
  ApiAuth ->
  Text ->
  UpdateArticleRequest ->
  m UpdateArticleResponse
handler (ApiAuth userId _) slug (UpdateArticleRequest input) = do
  result <- ArticleUseCase.updateArticle toCommand
  case result of
    Right result' -> do
      let params = Query.GetProfileParams Nothing result'.authorUsername
      profile <- QueryService.getProfile params `whenNothingM` throwError (notFound' "User not found")
      pure $ UpdateArticleResponse $ toArticle result' profile
    Left err -> do
      katipAddContext (sl "error" err) $ do
        $(logTM) ErrorS "updateArticle error"
      throwError $ toError err
 where
  toCommand :: ArticleUseCase.UpdateArticleCommand
  toCommand =
    UpdateArticleCommand
      { userId = userId
      , slug = slug
      , title = input.title
      , description = input.description
      , body = input.body
      }
  toArticle :: ArticleUseCase.UpdateArticleResult -> Profile -> Article
  toArticle result author =
    Article
      { slug = result.slug
      , title = result.title
      , description = result.description
      , body = result.body
      , tagList = result.tags
      , createdAt = result.createdAt
      , updatedAt = result.updatedAt
      , favorited = result.favorited
      , favoritesCount = result.favoritesCount
      , author = author
      }