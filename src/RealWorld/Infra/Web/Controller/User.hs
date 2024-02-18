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
import RealWorld.Domain.User.Gateway.Password (PasswordGateway)
import RealWorld.Domain.User.Gateway.Token (TokenGateway)
import qualified RealWorld.Domain.User.Gateway.Token as TokenGateway
import RealWorld.Domain.User.Repo (UserRepository)
import RealWorld.Domain.User.Types (Token (unToken))
import qualified RealWorld.Domain.User.UseCase as UserUseCase
import RealWorld.Infra.Json ()
import RealWorld.Infra.Web.ErrorResponse
  ( ErrorResponse,
    invalid,
    notFound,
    unauthorized,
  )
import RealWorld.Infra.Web.Util (withToken, (!?))
import RealWorld.Query.Types (Query, token)
import qualified RealWorld.Query.Types as Query
import Relude hiding (null, optional)
import Web.Scotty.Trans (ActionT, json, jsonData, param, raise)

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
  (MonadIO m, UserRepository m, TokenGateway m, PasswordGateway m) =>
  ActionT ErrorResponse m ()
authentication = do
  UserInput AuthenticationInput {..} <- jsonData
  case UserUseCase.mkAuthenticationCommand email password of
    Nothing -> raise $ invalid "Login failed. Please check your email or password."
    Just command -> do
      result <- lift $ UserUseCase.authentication command
      case result of
        Right authorizedUser -> json $ object ["user" .= authorizedUser]
        Left err -> raise $ invalid $ show err

data RegistrationInput = RegistrationInput
  { email :: Text,
    username :: Text,
    password :: Text
  }
  deriving (Show, Generic, FromJSON)

registration ::
  (MonadIO m, UserRepository m, TokenGateway m, PasswordGateway m, Tx m) =>
  ActionT ErrorResponse m ()
registration = do
  UserInput RegistrationInput {..} <- jsonData
  case UserUseCase.mkRegistrationCommand username email password of
    Nothing -> raise $ invalid "Registration failed. Please check your email, username or password."
    Just command -> do
      result <- lift $ UserUseCase.registration command
      case result of
        Right authorizedUser -> json $ object ["user" .= authorizedUser]
        Left err -> raise $ invalid $ show err

getCurrentUser :: (MonadIO m, Query m, TokenGateway m) => ActionT ErrorResponse m ()
getCurrentUser = do
  withToken $ \token -> do
    userId <- TokenGateway.verify token !? unauthorized "Unauthorized"
    let params = Query.GetCurrentUserParams $ show userId
    user <- Query.getCurrentUser params !? notFound "User not found"
    json $ object ["user" .= user {token = unToken token}]

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
  (MonadIO m, UserRepository m, TokenGateway m, PasswordGateway m, Tx m) =>
  ActionT ErrorResponse m ()
updateUser = do
  withToken $ \token -> do
    UserInput UpdateUserInput {..} <- jsonData
    case UserUseCase.mkUpdateUserCommand token username email password bio image of
      Nothing -> raise $ invalid "Update failed. Please check your email, username or password."
      Just command -> do
        result <- lift $ UserUseCase.updateUser command
        case result of
          Right authorizedUser -> json $ object ["user" .= authorizedUser]
          Left err -> raise $ unauthorized $ show err

getProfile :: (MonadIO m, Query m, TokenGateway m) => ActionT ErrorResponse m ()
getProfile = do
  withToken $ \token -> do
    userId <- TokenGateway.verify token !? unauthorized "Unauthorized"
    username <- param "username"
    let params = Query.GetProfileParams (Just $ show userId) username
    profile <- Query.getProfile params !? notFound "Profile not found"
    json $ object ["profile" .= profile]

follow :: (MonadIO m, UserRepository m, TokenGateway m, Tx m) => ActionT ErrorResponse m ()
follow = do
  withToken $ \token -> do
    username <- param "username"
    case UserUseCase.mkFollowUserCommand token username of
      Nothing -> raise $ invalid "Follow failed. Please check the username."
      Just command -> do
        result <- lift $ UserUseCase.followUser command
        case result of
          Right profile -> json $ object ["profile" .= profile]
          Left err -> raise $ invalid $ show err

unfollow :: (MonadIO m, UserRepository m, TokenGateway m, Tx m) => ActionT ErrorResponse m ()
unfollow = do
  withToken $ \token -> do
    username <- param "username"
    case UserUseCase.mkUnfollowUserCommand token username of
      Nothing -> raise $ invalid "Unfollow failed. Please check the username."
      Just command -> do
        result <- lift $ UserUseCase.unfollowUser command
        case result of
          Right profile -> json $ object ["profile" .= profile]
          Left err -> raise $ invalid $ show err