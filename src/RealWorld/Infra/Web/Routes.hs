{-# LANGUAGE FlexibleContexts #-}

module RealWorld.Infra.Web.Routes where

import Katip (KatipContext)
import Network.Wai.Middleware.RequestLogger (logStdout)
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository)
import RealWorld.Domain.Adapter.Repository.CommentRepository (CommentRepository)
import RealWorld.Domain.Adapter.Repository.FavoriteRepository (FavoriteRepository)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Query.Service (QueryService)
import qualified RealWorld.Infra.Web.Controller.Article as Article
import qualified RealWorld.Infra.Web.Controller.User as User
import RealWorld.Infra.Web.ErrorResponse
  ( ErrorResponse (ErrorResponse),
  )
import qualified RealWorld.Infra.Web.ErrorResponse as ErrorResponse
import Relude hiding (get, put)
import Web.Scotty.Trans
  ( ScottyT,
    defaultHandler,
    delete,
    get,
    json,
    middleware,
    notFound,
    post,
    put,
    raise,
    status,
  )

routes ::
  ( KatipContext m,
    TxManager m,
    ArticleRepository m,
    UserRepository m,
    CommentRepository m,
    FavoriteRepository m,
    TokenGateway m,
    PasswordGateway m,
    QueryService m
  ) =>
  ScottyT ErrorResponse m ()
routes = do
  middleware logStdout

  defaultHandler $ \(ErrorResponse status' errors) -> do
    status status'
    json errors

  post "/api/users/login" User.authentication

  post "/api/users" User.registration

  get "/api/user" User.getCurrentUser

  put "/api/user" User.updateUser

  get "/api/profiles/:username" User.getProfile

  post "/api/profiles/:username/follow" User.follow

  delete "/api/profiles/:username/follow" User.unfollow

  get "/api/articles" Article.listArticles

  get "/api/articles/feed" Article.feedArticles

  get "/api/articles/:slug" Article.getArticle

  post "/api/articles" Article.createArticle

  put "/api/articles/:slug" Article.updateArticle

  delete "/api/articles/:slug" Article.deleteArticle

  post "/api/articles/:slug/comments" Article.addComments

  get "/api/articles/:slug/comments" Article.getComments

  delete "/api/articles/:slug/comments/:comment-id" Article.deleteComment

  post "/api/articles/:slug/favorite" Article.favorite

  delete "/api/articles/:slug/favorite" Article.unfavorite

  get "/api/tags" Article.getTags

  notFound $ raise $ ErrorResponse.notFound "API not found"
