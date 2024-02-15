{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Conduit.Infra.Web.Controller.User where

import Conduit.Domain.Repo (Tx)
import Conduit.Domain.User.Entity (mkAuthenticationCommand, mkRegistrationCommand)
import Conduit.Domain.User.Gateway.Token (TokenGateway)
import Conduit.Domain.User.Repo (UserRepository)
import Conduit.Domain.User.Service.Password (PasswordService)
import qualified Conduit.Domain.User.UseCase as UserUseCase
import Conduit.Infra.Json ()
import Conduit.Infra.Web.ErrorResponse
  ( ErrorResponse,
    raiseInvalidate,
    raiseUnauthorized,
  )
import Conduit.Util.Controller (withToken)
import Data.Aeson (FromJSON)
import Relude
import Web.Scotty.Trans (ActionT, json, jsonData)

data RegistrationInput = RegistrationInput
  { email :: Text,
    userName :: Text,
    password :: Text
  }
  deriving (Show, Generic, FromJSON)

registration ::
  (MonadIO m, UserRepository m, TokenGateway m, PasswordService m, Tx m) =>
  ActionT ErrorResponse m ()
registration = do
  RegistrationInput {..} <- jsonData
  case mkRegistrationCommand userName email password of
    Left err -> raiseInvalidate $ show err
    Right command -> do
      result <- lift $ UserUseCase.registration command
      case result of
        Right authorizedUser -> json authorizedUser
        Left err -> raiseInvalidate $ show err

data AuthenticationInput = AuthenticationInput
  { email :: Text,
    password :: Text
  }
  deriving (Show, Generic, FromJSON)

authentication ::
  (MonadIO m, UserRepository m, TokenGateway m, PasswordService m) =>
  ActionT ErrorResponse m ()
authentication = do
  AuthenticationInput {..} <- jsonData
  case mkAuthenticationCommand email password of
    Left err -> raiseInvalidate $ show err
    Right command -> do
      result <- lift $ UserUseCase.authentication command
      case result of
        Right authorizedUser -> json authorizedUser
        Left err -> raiseInvalidate $ show err

getCurrentUser :: (MonadIO m, UserRepository m, TokenGateway m) => ActionT ErrorResponse m ()
getCurrentUser = do
  withToken $ \token -> do
    result <- lift $ UserUseCase.getCurrentUser token
    case result of
      Right authorizedUser -> json authorizedUser
      Left err -> raiseUnauthorized $ show err
