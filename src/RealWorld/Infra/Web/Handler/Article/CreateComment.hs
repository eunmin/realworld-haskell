{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.CreateComment where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Swagger (ToSchema)
import Effectful (Eff, IOE)
import qualified Effectful as Eff
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Katip
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository)
import RealWorld.Domain.Adapter.Repository.CommentRepository (CommentRepository)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.Article.UseCase (AddCommentsError (..))
import qualified RealWorld.Domain.Command.Article.UseCase as ArticleUseCase
import RealWorld.Domain.Query.Data (Comment (..), Profile)
import qualified RealWorld.Domain.Query.Data as Query
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.ErrorResponse (badRequest, notFound')
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (Capture, JSON, Post, ReqBody, ServerError, (:>))

type Route =
  "articles"
    :> Capture "slug" Text
    :> "comments"
    :> ReqBody '[JSON] CreateCommentsRequest
    :> Post '[JSON] CreateCommentsResponse

data CreateCommentsRequest = CreateCommentsRequest
  { comment :: CreateCommentsInput
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreateCommentsInput = CreateCommentsInput
  { body :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data CreateCommentsResponse = CreateCommentsResponse
  { comment :: Comment
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

toError :: ArticleUseCase.AddCommentsError -> ServerError
toError AddCommentsErrorInvalidUserId = badRequest "Invalid User Id"
toError AddCommentsErrorInvalidBody = badRequest "Invalid body"
toError AddCommentsErrorArticleNotFound = notFound' "Article not found"
toError AddCommentsErrorAuthorNotFound = notFound' "Author not found"
toError AddCommentsErrorInvalidSlug = badRequest "Invalid slug"

handler ::
  ( IOE Eff.:> es
  , KatipE Eff.:> es
  , ArticleRepository Eff.:> es
  , TxManager Eff.:> es
  , UserRepository Eff.:> es
  , CommentRepository Eff.:> es
  , QueryService Eff.:> es
  , Error ServerError Eff.:> es
  ) =>
  ApiAuth ->
  Text ->
  CreateCommentsRequest ->
  Eff es CreateCommentsResponse
handler (ApiAuth userId _) slug (CreateCommentsRequest input) = do
  result <- ArticleUseCase.addComments toCommand
  case result of
    Right result' -> do
      let params = Query.GetProfileParams Nothing result'.authorUsername
      profile <- QueryService.getProfile params `whenNothingM` throwError (notFound' "Article not found")
      pure $ CreateCommentsResponse $ toComment result' input.body profile
    Left err -> do
      katipAddContext (sl "error" err) $ do
        $(logTM) ErrorS "addComments error"
      throwError $ toError err
 where
  toCommand :: ArticleUseCase.AddCommentsCommand
  toCommand =
    ArticleUseCase.AddCommentsCommand
      { userId = userId
      , slug = slug
      , body = input.body
      }
  toComment :: ArticleUseCase.AddCommentsResult -> Text -> Profile -> Query.Comment
  toComment result body profile =
    Comment
      { commentId = result.commentId
      , createdAt = result.createdAt
      , updatedAt = Nothing
      , body = body
      , author = profile
      }