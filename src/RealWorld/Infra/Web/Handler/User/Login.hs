{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Web.Handler.User.Login where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Swagger (ToSchema)
import Effectful (Eff)
import qualified Effectful as Eff
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Katip
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
import Servant (JSON, Post, ReqBody, (:>))
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
  ( KatipE Eff.:> es
  , UserRepository Eff.:> es
  , TokenGateway Eff.:> es
  , PasswordGateway Eff.:> es
  , Error ServerError Eff.:> es
  ) =>
  LoginRequest ->
  Eff es LoginResponse
handler (LoginRequest input) = do
  result <- UserUseCase.authentication toCommand
  case result of
    Right result' -> pure $ LoginResponse $ toUser result'
    Left err -> do
      katipAddContext (sl "error" err) $ do
        $(logTM) ErrorS "authentication error"
      throwError $ toError err
 where
  toCommand :: UserUseCase.AuthenticationCommand
  toCommand =
    UserUseCase.AuthenticationCommand
      { email = input.email
      , password = input.password
      }
  toUser :: AuthenticationResult -> User
  toUser result =
    User
      { email = input.email
      , token = result.token
      , username = result.username
      , bio = result.bio
      , image = result.image
      }