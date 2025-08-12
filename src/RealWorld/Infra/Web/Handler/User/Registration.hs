{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Web.Handler.User.Registration where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Swagger (ToSchema)
import Effectful (Eff, IOE)
import qualified Effectful as Eff
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Katip
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.User.UseCase (RegistrationError (..))
import qualified RealWorld.Domain.Command.User.UseCase as UserUseCase
import RealWorld.Domain.Query.Data (User (..))
import RealWorld.Infra.Converter.Aeson ()
import RealWorld.Infra.Web.ErrorResponse (badRequest)
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
  ( IOE Eff.:> es
  , KatipE Eff.:> es
  , UserRepository Eff.:> es
  , TokenGateway Eff.:> es
  , PasswordGateway Eff.:> es
  , TxManager Eff.:> es
  , Error ServerError Eff.:> es
  ) =>
  RegistrationRequest ->
  Eff es RegistrationResponse
handler (RegistrationRequest input) = do
  result <- UserUseCase.registration toCommand
  case result of
    Right result' -> pure $ RegistrationResponse $ toUser result'
    Left err -> do
      katipAddContext (sl "error" err) $ do
        $(logTM) ErrorS "registration error"
      throwError $ toError err
 where
  toCommand :: UserUseCase.RegistrationCommand
  toCommand =
    UserUseCase.RegistrationCommand
      { username = input.username
      , email = input.email
      , password = input.password
      }
  toUser (UserUseCase.RegistrationResult token) =
    User
      { email = input.email
      , token = token
      , username = input.username
      , bio = ""
      , image = Nothing
      }
