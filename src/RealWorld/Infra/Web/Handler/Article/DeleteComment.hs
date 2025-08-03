{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.DeleteComment where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Swagger (ToSchema)
import Data.Time (UTCTime)
import Katip
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository)
import RealWorld.Domain.Adapter.Repository.CommentRepository (CommentRepository)
import RealWorld.Domain.Adapter.Repository.FavoriteRepository (FavoriteRepository)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.Article.UseCase (AddCommentsError (..), CreateArticleCommand (..), CreateArticleError (..), DeleteArticleError (..), DeleteCommentError (..), UpdateArticleCommand (..), UpdateArticleError (..))
import qualified RealWorld.Domain.Command.Article.UseCase as ArticleUseCase
import RealWorld.Domain.Query.Data (Article (..), Comment (..), GetProfileParams (..), Profile)
import qualified RealWorld.Domain.Query.Data as Query
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.ErrorResponse (badRequest, notFound')
import RealWorld.Infra.Web.Handler.Types (ArticleWrapper, CommentWrapper, ProfileWrapper, UserWrapper)
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (Capture, Delete, Get, JSON, NoContent (..), Post, Put, ReqBody, ServerError, (:>))

type Route =
  "articles"
    :> Capture "slug" Text
    :> "comments"
    :> Capture "comment-id" Text
    :> Delete '[JSON] NoContent

instance ToJSON DeleteCommentError

toError :: ArticleUseCase.DeleteCommentError -> ServerError
toError DeleteCommentErrorInvalidUserId = badRequest "Invalid User Id"
toError DeleteCommentErrorInvalidSlug = badRequest "Invalid slug"
toError DeleteCommentErrorInvalidCommentId = badRequest "Invalid comment id"
toError DeleteCommentErrorArticleNotFound = notFound' "Article not found"
toError DeleteCommentErrorCommentNotFound = notFound' "Comment not found"
toError DeleteCommentErrorDeletePermissionDenied = badRequest "Delete permission denied"

handler ::
  (KatipContext m, ArticleRepository m, CommentRepository m, TxManager m, MonadError ServerError m) =>
  ApiAuth ->
  Text ->
  Text ->
  m NoContent
handler (ApiAuth userId _) slug commentId = do
  result <- ArticleUseCase.deleteComment $ toCommand
  case result of
    Right _ -> pure NoContent
    Left err -> do
      katipAddContext (sl "error" err) $ do
        $(logTM) ErrorS "deleteComment error"
      throwError $ toError err
 where
  toCommand :: ArticleUseCase.DeleteCommentCommand
  toCommand =
    ArticleUseCase.DeleteCommentCommand
      { userId = userId
      , slug = slug
      , commentId = commentId
      }