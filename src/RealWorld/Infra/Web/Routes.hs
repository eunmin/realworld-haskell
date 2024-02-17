{-# LANGUAGE FlexibleContexts #-}

module RealWorld.Infra.Web.Routes where

import Network.Wai.Middleware.RequestLogger (logStdout)
import RealWorld.Domain.Repo (Tx)
import RealWorld.Domain.User.Gateway.Token (TokenGateway)
import RealWorld.Domain.User.Repo (UserRepository)
import RealWorld.Domain.User.Service.Password (PasswordService)
import qualified RealWorld.Infra.Web.Controller.User as User
import RealWorld.Infra.Web.ErrorResponse
  ( ErrorResponse (ErrorResponse),
    raiseNotFound,
  )
import Relude hiding (get, put)
import Web.Scotty.Trans
  ( ScottyT,
    defaultHandler,
    get,
    json,
    middleware,
    notFound,
    post,
    put,
    status,
  )

routes ::
  (MonadIO m, Tx m, UserRepository m, TokenGateway m, PasswordService m) =>
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

  notFound $ raiseNotFound "Not found"
