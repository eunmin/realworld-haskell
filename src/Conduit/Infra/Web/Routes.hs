{-# LANGUAGE FlexibleContexts #-}

module Conduit.Infra.Web.Routes where

import Conduit.Domain.Repo (Tx)
import Conduit.Domain.User.Gateway.Token (TokenGateway)
import Conduit.Domain.User.Repo (UserRepository)
import Conduit.Domain.User.Service.Password (PasswordService)
import qualified Conduit.Infra.Web.Controller.User as User
import Conduit.Infra.Web.ErrorResponse
  ( ErrorResponse (ErrorResponse),
    raiseNotFound,
  )
import Network.Wai.Middleware.RequestLogger (logStdout)
import Relude hiding (get)
import Web.Scotty.Trans
  ( ScottyT,
    defaultHandler,
    get,
    json,
    middleware,
    notFound,
    post,
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

  notFound $ raiseNotFound "Not found"
