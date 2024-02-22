{-# LANGUAGE FlexibleContexts #-}

module RealWorld.Infra.Web.Routes where

import Network.Wai.Middleware.RequestLogger (logStdout)
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository)
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
  ( MonadIO m,
    TxManager m,
    ArticleRepository m,
    UserRepository m,
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

  post "/api/articles" Article.createArticle

  put "/api/articles/:slug" Article.updateArticle

  delete "/api/articles/:slug" Article.deleteArticle

  notFound $ raise $ ErrorResponse.notFound "API not found"
