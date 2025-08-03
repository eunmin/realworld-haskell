{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.User.UpdateUser where

import Control.Monad.Except (MonadError (..))
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
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.User.UseCase (UpdateUserError (..))
import qualified RealWorld.Domain.Command.User.UseCase as UserUseCase
import RealWorld.Domain.Query.Data (User (..))
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.ErrorResponse (badRequest, notFound')
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (JSON, Put, ReqBody, ServerError, (:>))

type Route =
  "users"
    :> ReqBody '[JSON] UpdateUserRequest
    :> Put '[JSON] UpdateUserResponse

data UpdateUserRequest = UpdateUserRequest
  { user :: UpdateUserParams
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data UpdateUserParams = UpdateUserParams
  { email :: Maybe Text
  , username :: Maybe Text
  , password :: Maybe Text
  , bio :: Maybe Text
  , image :: Maybe (Maybe Text)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data UpdateUserResponse = UpdateUserResponse
  { user :: User
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

toError :: UpdateUserError -> ServerError
toError UpdateUserErrorInvalidUserId = badRequest "Invalid user id"
toError UpdateUserErrorInvalidUsername = badRequest "Invalid username"
toError UpdateUserErrorInvalidEmail = badRequest "Invalid email"
toError UpdateUserErrorInvalidPassword = badRequest "Invalid password"
toError UpdateUserErrorUserNotFound = notFound' "User not found"
toError UpdateUserErrorUsernameAlreadyExists = badRequest "Username already exists"

handler ::
  (KatipContext m, UserRepository m, PasswordGateway m, TxManager m, MonadError ServerError m) =>
  ApiAuth ->
  UpdateUserRequest ->
  m UpdateUserResponse
handler (ApiAuth userId token) (UpdateUserRequest input) = do
  result <- UserUseCase.updateUser $ toCommand
  case result of
    Right result' -> pure $ UpdateUserResponse $ toUser result'
    Left err -> do
      katipAddContext (sl "error" err) $ do
        $(logTM) ErrorS "updateUser error"
      throwError $ toError err
 where
  toCommand :: UserUseCase.UpdateUserCommand
  toCommand =
    UserUseCase.UpdateUserCommand
      { userId = userId
      , username = input.username
      , email = input.email
      , password = input.password
      , bio = input.bio
      , image = input.image
      }
  toUser :: UserUseCase.UpdateUserResult -> User
  toUser result =
    User
      { email = result.email
      , token = token
      , username = result.username
      , bio = result.bio
      , image = result.image
      }