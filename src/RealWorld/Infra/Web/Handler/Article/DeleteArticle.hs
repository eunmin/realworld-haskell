{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.DeleteArticle where

import Effectful (Eff)
import qualified Effectful as Eff
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Katip (
  KatipE,
  Severity (ErrorS),
  katipAddContext,
  logTM,
  sl,
 )
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository)
import RealWorld.Domain.Command.Article.UseCase (DeleteArticleError (..))
import qualified RealWorld.Domain.Command.Article.UseCase as ArticleUseCase
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.ErrorResponse (badRequest, notFound')
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (Capture, Delete, JSON, NoContent (..), ServerError, (:>))

type Route =
  "articles"
    :> Capture "slug" Text
    :> Delete '[JSON] NoContent

toError :: ArticleUseCase.DeleteArticleError -> ServerError
toError DeleteArticleErrorInvalidUserId = badRequest "Invalid User Id"
toError DeleteArticleErrorInvalidSlug = badRequest "Invalid slug"
toError DeleteArticleErrorArticleNotFound = notFound' "Article not found"
toError DeleteArticleErrorDeletePermissionDenied = badRequest "Delete permission denied"

handler ::
  ( KatipE Eff.:> es
  , ArticleRepository Eff.:> es
  , TxManager Eff.:> es
  , Error ServerError Eff.:> es
  ) =>
  ApiAuth ->
  Text ->
  Eff es NoContent
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