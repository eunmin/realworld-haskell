{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.CreateArticle where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Swagger (ToSchema)
import Katip
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.Article.UseCase (CreateArticleCommand (..), CreateArticleError (..))
import qualified RealWorld.Domain.Command.Article.UseCase as ArticleUseCase
import RealWorld.Domain.Query.Data (Article (..), GetProfileParams (..), Profile)
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.ErrorResponse (badRequest, notFound')
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (JSON, Post, ReqBody, ServerError, (:>))

type Route =
  "articles"
    :> ReqBody '[JSON] CreateArticleRequest
    :> Post '[JSON] CreateArticleResponse

data CreateArticleRequest = CreateArticleRequest
  { article :: CreateArticleInput
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreateArticleInput = CreateArticleInput
  { title :: Text
  , description :: Text
  , body :: Text
  , tagList :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreateArticleResponse = CreateArticleResponse
  { article :: Article
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

toError :: ArticleUseCase.CreateArticleError -> ServerError
toError CreateArticleErrorInvalidTitle = badRequest "Invalid title"
toError CreateArticleErrorInvalidUserId = badRequest "Invalid user id"
toError CreateArticleErrorInvalidBody = badRequest "Invalid body"
toError CreateArticleErrorInvalidDescription = badRequest "Invalid description"
toError CreateArticleErrorInvalidTag = badRequest "Invalid tag"
toError CreateArticleErrorAuthorNotFound = notFound' "Author not found"

handler ::
  ( KatipContext m
  , ArticleRepository m
  , UserRepository m
  , TxManager m
  , QueryService m
  , MonadError ServerError m
  ) =>
  ApiAuth ->
  CreateArticleRequest ->
  m CreateArticleResponse
handler (ApiAuth userId _) (CreateArticleRequest input) = do
  result <- ArticleUseCase.createArticle toCommand
  case result of
    Right result' -> do
      let params = GetProfileParams Nothing result'.authorUsername
      profile <- QueryService.getProfile params `whenNothingM` throwError (notFound' "User not found")
      pure $ CreateArticleResponse $ toArticle result' profile
    Left err -> do
      katipAddContext (sl "error" err) $ do
        $(logTM) ErrorS "createArticle error"
      throwError $ toError err
 where
  toCommand :: ArticleUseCase.CreateArticleCommand
  toCommand =
    CreateArticleCommand
      { title = input.title
      , description = input.description
      , body = input.body
      , tagList = input.tagList
      , userId = userId
      }
  toArticle :: ArticleUseCase.CreateArticleResult -> Profile -> Article
  toArticle result author =
    Article
      { slug = result.slug
      , title = input.title
      , description = input.description
      , body = input.body
      , tagList = input.tagList
      , createdAt = result.createdAt
      , updatedAt = Nothing
      , favorited = False
      , favoritesCount = 0
      , author = author
      }