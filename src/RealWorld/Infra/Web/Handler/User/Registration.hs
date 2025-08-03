{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Web.Handler.User.Registration where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Swagger (ToSchema)
import Katip
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.User.UseCase (RegistrationError (..))
import qualified RealWorld.Domain.Command.User.UseCase as UserUseCase
import RealWorld.Domain.Query.Data (User (..))
import RealWorld.Infra.Converter.Aeson ()
import RealWorld.Infra.Web.ErrorResponse (badRequest)
import RealWorld.Infra.Web.Handler.Types ()
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (JSON, Post, ReqBody, ServerError, (:>))

type Route =
  "users"
    :> ReqBody '[JSON] RegistrationRequest
    :> Post '[JSON] RegistrationResponse

data RegistrationRequest = RegistrationRequest
  { user :: RegistrationParams
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema RegistrationRequest

data RegistrationParams = RegistrationParams
  { email :: Text
  , username :: Text
  , password :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema RegistrationParams

data RegistrationResponse = RegistrationResponse
  { user :: User
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema RegistrationResponse

toError :: RegistrationError -> ServerError
toError RegistrationErrorUsernameAlreadyExists = badRequest "Username already exists"
toError RegistrationErrorEmailAlreadyExists = badRequest "User already exists"
toError RegistrationErrorInvalidEmail = badRequest "Invalid email"
toError RegistrationErrorInvalidUsername = badRequest "Invalid username"
toError RegistrationErrorInvalidPassword = badRequest "Invalid password"

handler ::
  ( KatipContext m
  , UserRepository m
  , TokenGateway m
  , PasswordGateway m
  , TxManager m
  , MonadError ServerError m
  ) =>
  RegistrationRequest ->
  m RegistrationResponse
handler (RegistrationRequest input) = do
  result <- UserUseCase.registration $ toCommand input
  case result of
    Right result' -> pure $ RegistrationResponse $ toUser input result'
    Left err -> do
      katipAddContext (sl "error" err) $ do
        $(logTM) ErrorS "registration error"
      throwError $ toError err
 where
  toCommand :: RegistrationParams -> UserUseCase.RegistrationCommand
  toCommand request =
    UserUseCase.RegistrationCommand
      { username = request.username
      , email = request.email
      , password = request.password
      }
  toUser request (UserUseCase.RegistrationResult token) =
    User
      { email = request.email
      , token = token
      , username = request.username
      , bio = ""
      , image = Nothing
      }
