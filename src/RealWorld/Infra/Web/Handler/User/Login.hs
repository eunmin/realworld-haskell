{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Web.Handler.User.Login where

import Control.Monad.Except (MonadError)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Swagger (ToSchema)
import Katip (
  KatipContext,
  Severity (ErrorS),
  katipAddContext,
  logTM,
  sl,
 )
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.User.Entity.User ()
import RealWorld.Domain.Command.User.UseCase (AuthenticationError (..), AuthenticationResult)
import qualified RealWorld.Domain.Command.User.UseCase as UserUseCase
import RealWorld.Domain.Query.Data (User (..))
import RealWorld.Infra.Web.ErrorResponse (badRequest, notFound')
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (JSON, Post, ReqBody, throwError, (:>))
import Servant.Server (ServerError)

type Route =
  "users"
    :> "login"
    :> ReqBody '[JSON] LoginRequest
    :> Post '[JSON] LoginResponse

data LoginRequest = LoginRequest
  { user :: LoginParams
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data LoginParams = LoginParams
  { email :: Text
  , password :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance ToJSON AuthenticationError

data LoginResponse = LoginResponse
  { user :: User
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

toError :: AuthenticationError -> ServerError
toError AuthenticationErrorUserNotFound = notFound' "User not found"
toError AuthenticationErrorInvalidPassword = badRequest "Invalid password"
toError AuthenticationErrorInvalidEmail = badRequest "Invalid email"

handler ::
  ( KatipContext m
  , UserRepository m
  , TokenGateway m
  , PasswordGateway m
  , MonadError ServerError m
  ) =>
  LoginRequest ->
  m LoginResponse
handler (LoginRequest input) = do
  result <- UserUseCase.authentication $ toCommand input
  case result of
    Right result' -> pure $ LoginResponse $ toUser input result'
    Left err -> do
      katipAddContext (sl "error" err) $ do
        $(logTM) ErrorS "authentication error"
      throwError $ toError err
 where
  toCommand :: LoginParams -> UserUseCase.AuthenticationCommand
  toCommand request =
    UserUseCase.AuthenticationCommand
      { email = request.email
      , password = request.password
      }
  toUser :: LoginParams -> AuthenticationResult -> User
  toUser request result =
    User
      { email = request.email
      , token = result.token
      , username = result.username
      , bio = result.bio
      , image = result.image
      }