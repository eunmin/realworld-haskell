{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module RealWorld.Infra.Web.Controller.User where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    object,
    withObject,
    (.:!),
  )
import RealWorld.Domain.Repo (Tx)
import qualified RealWorld.Domain.User.Entity as User
import RealWorld.Domain.User.Gateway.Token (TokenGateway)
import RealWorld.Domain.User.Repo (UserRepository)
import RealWorld.Domain.User.Service.Password (PasswordService)
import qualified RealWorld.Domain.User.UseCase as UserUseCase
import RealWorld.Infra.Json ()
import RealWorld.Infra.Web.ErrorResponse
  ( ErrorResponse,
    raiseInvalidate,
    raiseUnauthorized,
  )
import RealWorld.Infra.Web.Util (withToken)
import Relude hiding (null, optional)
import Web.Scotty.Trans (ActionT, json, jsonData)

data UserInput a = UserInput
  { user :: a
  }
  deriving (Show, Generic, FromJSON)

data AuthenticationInput = AuthenticationInput
  { email :: Text,
    password :: Text
  }
  deriving (Show, Generic, FromJSON)

authentication ::
  (MonadIO m, UserRepository m, TokenGateway m, PasswordService m) =>
  ActionT ErrorResponse m ()
authentication = do
  UserInput AuthenticationInput {..} <- jsonData
  case User.mkAuthenticationCommand email password of
    Left err -> raiseInvalidate $ show err
    Right command -> do
      result <- lift $ UserUseCase.authentication command
      case result of
        Right authorizedUser -> json $ object ["user" .= authorizedUser]
        Left err -> raiseInvalidate $ show err

data RegistrationInput = RegistrationInput
  { email :: Text,
    username :: Text,
    password :: Text
  }
  deriving (Show, Generic, FromJSON)

registration ::
  (MonadIO m, UserRepository m, TokenGateway m, PasswordService m, Tx m) =>
  ActionT ErrorResponse m ()
registration = do
  UserInput RegistrationInput {..} <- jsonData
  case User.mkRegistrationCommand username email password of
    Left err -> raiseInvalidate $ show err
    Right command -> do
      result <- lift $ UserUseCase.registration command
      case result of
        Right authorizedUser -> json $ object ["user" .= authorizedUser]
        Left err -> raiseInvalidate $ show err

getCurrentUser :: (MonadIO m, UserRepository m, TokenGateway m) => ActionT ErrorResponse m ()
getCurrentUser = do
  withToken $ \token -> do
    result <- lift $ UserUseCase.getCurrentUser token
    case result of
      Right authorizedUser -> json $ object ["user" .= authorizedUser]
      Left err -> raiseUnauthorized $ show err

data UpdateUserInput = UpdateUserInput
  { email :: Maybe Text,
    username :: Maybe Text,
    password :: Maybe Text,
    bio :: Maybe Text,
    image :: Maybe (Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON UpdateUserInput where
  parseJSON = withObject "UpdateUserInput" $ \value -> do
    UpdateUserInput
      <$> value
      .:! "email"
      <*> value
      .:! "username"
      <*> value
      .:! "password"
      <*> value
      .:! "bio"
      <*> value
      .:! "image"

updateUser ::
  (MonadIO m, UserRepository m, TokenGateway m, PasswordService m, Tx m) =>
  ActionT ErrorResponse m ()
updateUser = do
  withToken $ \token -> do
    UserInput UpdateUserInput {..} <- jsonData
    case User.mkUpdateUserCommand token username email password bio image of
      Right command -> do
        result <- lift $ UserUseCase.updateUser command
        case result of
          Right authorizedUser -> json $ object ["user" .= authorizedUser]
          Left err -> raiseUnauthorized $ show err
      Left err -> raiseInvalidate $ show err
