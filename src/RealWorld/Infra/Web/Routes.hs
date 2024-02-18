{-# LANGUAGE FlexibleContexts #-}

module RealWorld.Infra.Web.Routes where

import Network.Wai.Middleware.RequestLogger (logStdout)
import RealWorld.Domain.Repo (Tx)
import RealWorld.Domain.User.Gateway.Password (PasswordGateway)
import RealWorld.Domain.User.Gateway.Token (TokenGateway)
import RealWorld.Domain.User.Repo (UserRepository)
import qualified RealWorld.Infra.Web.Controller.User as User
import RealWorld.Infra.Web.ErrorResponse
  ( ErrorResponse (ErrorResponse),
  )
import qualified RealWorld.Infra.Web.ErrorResponse as ErrorResponse
import RealWorld.Query.Types (Query)
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
  (MonadIO m, Tx m, UserRepository m, TokenGateway m, PasswordGateway m, Query m) =>
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

  notFound $ raise $ ErrorResponse.notFound "API not found"
