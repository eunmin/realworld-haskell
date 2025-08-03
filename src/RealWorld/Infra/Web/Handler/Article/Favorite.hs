{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.Favorite where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Swagger (ToSchema)
import Katip
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository)
import RealWorld.Domain.Adapter.Repository.FavoriteRepository (FavoriteRepository)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.Article.UseCase (FavoriteArticleError (..))
import qualified RealWorld.Domain.Command.Article.UseCase as ArticleUseCase
import RealWorld.Domain.Query.Data (Article (..), Profile)
import qualified RealWorld.Domain.Query.Data as Query
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.ErrorResponse (badRequest, notFound')
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (Capture, JSON, Post, ServerError, (:>))

type Route =
  "articles"
    :> Capture "slug" Text
    :> "favorite"
    :> Post '[JSON] FavoriteArticleResponse

data FavoriteArticleResponse = FavoriteArticleResponse
  { article :: Article
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

toError :: ArticleUseCase.FavoriteArticleError -> ServerError
toError FavoriteArticleErrorInvalidUserId = badRequest "Invalid User Id"
toError FavoriteArticleErrorInvalidSlug = badRequest "Invalid slug"
toError FavoriteArticleErrorArticleNotFound = notFound' "Article not found"
toError FavroiteArticleErrorUserNotFound = notFound' "User not found"
toError FavoriteArticleErrorAlreadyFavorited = badRequest "Already favorited"

handler ::
  ( ArticleRepository m
  , FavoriteRepository m
  , UserRepository m
  , TxManager m
  , QueryService m
  , KatipContext m
  , MonadError ServerError m
  ) =>
  ApiAuth ->
  Text ->
  m FavoriteArticleResponse
handler (ApiAuth userId _) slug = do
  result <- ArticleUseCase.favoriteArticle toCommand
  case result of
    Right result' -> do
      let params = Query.GetProfileParams Nothing result'.authorUsername
      profile <- QueryService.getProfile params `whenNothingM` throwError (notFound' "Author not found")
      pure $ FavoriteArticleResponse $ toArticle result' profile
    Left err -> do
      katipAddContext (sl "error" err) $ do
        $(logTM) ErrorS "favorite error"
      throwError $ toError err
 where
  toCommand :: ArticleUseCase.FavoriteArticleCommand
  toCommand =
    ArticleUseCase.FavoriteArticleCommand
      { userId = userId
      , slug = slug
      }

  toArticle :: ArticleUseCase.FavoriteArticleResult -> Profile -> Article
  toArticle result author =
    Article
      { slug = result.slug
      , title = result.title
      , description = result.description
      , body = result.body
      , tagList = result.tags
      , createdAt = result.createdAt
      , updatedAt = result.updatedAt
      , favorited = True
      , favoritesCount = result.favoritesCount
      , author = author
      }