{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module RealWorld.Infra.Web.Controller.User where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    genericParseJSON,
    object,
    withObject,
    (.:!),
  )
import Data.Aeson.Casing (aesonDrop, camelCase)
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import qualified RealWorld.Domain.Adapter.Gateway.TokenGateway as TokenGateway
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import qualified RealWorld.Domain.Command.User.UseCase as UserUseCase
import RealWorld.Domain.Command.User.Value (Token (unToken))
import RealWorld.Domain.Query.Data (userToken)
import qualified RealWorld.Domain.Query.Data as Query
import RealWorld.Domain.Query.Service (QueryService)
import qualified RealWorld.Domain.Query.Service as QueryService
import RealWorld.Infra.Json ()
import RealWorld.Infra.Web.ErrorResponse
  ( ErrorResponse,
    invalid,
    notFound,
    unauthorized,
  )
import RealWorld.Infra.Web.Util (withToken, (!?))
import Relude hiding (null, optional)
import Web.Scotty.Trans (ActionT, json, jsonData, param, raise)

data UserInput a = UserInput
  { user :: a
  }
  deriving (Show, Generic, FromJSON)

----------------------------------------------------------------------------------------------------
-- Registration

data RegistrationInput = RegistrationInput
  { registrationInputEmail :: Text,
    registrationInputUsername :: Text,
    registrationInputPassword :: Text
  }
  deriving (Show, Generic)

instance FromJSON RegistrationInput where
  parseJSON = genericParseJSON $ aesonDrop 17 camelCase

registration ::
  (MonadIO m, UserRepository m, TokenGateway m, PasswordGateway m, TxManager m) =>
  ActionT ErrorResponse m ()
registration = do
  UserInput input <- jsonData
  result <- lift $ UserUseCase.registration $ toCommand input
  case result of
    Right result' -> json $ object ["user" .= toUser input result']
    Left err -> raise $ invalid $ show err
  where
    toCommand :: RegistrationInput -> UserUseCase.RegistrationCommand
    toCommand RegistrationInput {..} =
      UserUseCase.RegistrationCommand
        { registrationCommandUsername = registrationInputEmail,
          registrationCommandEmail = registrationInputUsername,
          registrationCommandPassword = registrationInputPassword
        }
    toUser :: RegistrationInput -> UserUseCase.RegistrationResult -> Query.User
    toUser RegistrationInput {..} (UserUseCase.RegistrationResult token) =
      Query.User
        { userEmail = registrationInputEmail,
          userToken = token,
          userUsername = registrationInputUsername,
          userBio = "",
          userImage = Nothing
        }

----------------------------------------------------------------------------------------------------
-- Authentication

data AuthenticationInput = AuthenticationInput
  { authenticationInputEmail :: Text,
    authenticationInputPassword :: Text
  }
  deriving (Show, Generic)

instance FromJSON AuthenticationInput where
  parseJSON = genericParseJSON $ aesonDrop 19 camelCase

authentication ::
  (MonadIO m, UserRepository m, TokenGateway m, PasswordGateway m) =>
  ActionT ErrorResponse m ()
authentication = do
  UserInput input <- jsonData
  result <- lift $ UserUseCase.authentication $ toCommand input
  case result of
    Right result' -> json $ object ["user" .= toUser input result']
    Left err -> raise $ invalid $ show err
  where
    toCommand :: AuthenticationInput -> UserUseCase.AuthenticationCommand
    toCommand AuthenticationInput {..} =
      UserUseCase.AuthenticationCommand
        { authenticationCommandEmail = authenticationInputEmail,
          authenticationCommandPassword = authenticationInputPassword
        }
    toUser :: AuthenticationInput -> UserUseCase.AuthenticationResult -> Query.User
    toUser AuthenticationInput {..} UserUseCase.AuthenticationResult {..} =
      Query.User
        { userEmail = authenticationInputEmail,
          userToken = authenticationResultToken,
          userUsername = authenticationResultUsername,
          userBio = authenticationResultBio,
          userImage = authenticationResultImage
        }

----------------------------------------------------------------------------------------------------
-- Get Current User

getCurrentUser :: (MonadIO m, QueryService m, TokenGateway m) => ActionT ErrorResponse m ()
getCurrentUser = do
  withToken $ \token -> do
    userId <- TokenGateway.verify token !? unauthorized "Unauthorized"
    let params = Query.GetCurrentUserParams $ show userId
    user <- QueryService.getCurrentUser params !? notFound "User not found"
    json $ object ["user" .= user {userToken = unToken token}]

----------------------------------------------------------------------------------------------------
-- Update User

data UpdateUserInput = UpdateUserInput
  { updateUserInputEmail :: Maybe Text,
    updateUserInputUsername :: Maybe Text,
    updateUserInputPassword :: Maybe Text,
    updateUserInputBio :: Maybe Text,
    updateUserInputImage :: Maybe (Maybe Text)
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
  (MonadIO m, UserRepository m, TokenGateway m, PasswordGateway m, TxManager m) =>
  ActionT ErrorResponse m ()
updateUser = do
  withToken $ \token -> do
    UserInput input <- jsonData
    result <- lift $ UserUseCase.updateUser $ toCommand input (unToken token)
    case result of
      Right result' -> json $ object ["user" .= toUser result']
      Left err -> raise $ unauthorized $ show err
  where
    toCommand :: UpdateUserInput -> Text -> UserUseCase.UpdateUserCommand
    toCommand UpdateUserInput {..} token =
      UserUseCase.UpdateUserCommand
        { updateUserCommandToken = token,
          updateUserCommandUserName = updateUserInputUsername,
          updateUserCommandEmail = updateUserInputEmail,
          updateUserCommandPassword = updateUserInputPassword,
          updateUserCommandBio = updateUserInputBio,
          updateUserCommandImage = updateUserInputImage
        }
    toUser :: UserUseCase.UpdateUserResult -> Query.User
    toUser UserUseCase.UpdateUserResult {..} =
      Query.User
        { userEmail = updateUserResultEmail,
          userToken = updateUserResultToken,
          userUsername = updateUserResultUsername,
          userBio = updateUserResultBio,
          userImage = updateUserResultImage
        }

----------------------------------------------------------------------------------------------------
-- Get Profile

getProfile :: (MonadIO m, QueryService m, TokenGateway m) => ActionT ErrorResponse m ()
getProfile = do
  withToken $ \token -> do
    userId <- TokenGateway.verify token !? unauthorized "Unauthorized"
    username <- param "username"
    let params = Query.GetProfileParams (Just $ show userId) username
    profile <- QueryService.getProfile params !? notFound "Profile not found"
    json $ object ["profile" .= profile]

----------------------------------------------------------------------------------------------------
-- Follow User

follow :: (MonadIO m, UserRepository m, TokenGateway m, TxManager m) => ActionT ErrorResponse m ()
follow = do
  withToken $ \token -> do
    username <- param "username"
    result <- lift $ UserUseCase.followUser $ toCommand (unToken token) username
    case result of
      Right result' -> json $ object ["profile" .= toProfile result']
      Left err -> raise $ invalid $ show err
  where
    toCommand :: Text -> Text -> UserUseCase.FollowUserCommand
    toCommand token username =
      UserUseCase.FollowUserCommand
        { followUserCommandToken = token,
          followUserCommandUsername = username
        }
    toProfile :: UserUseCase.FollowUserResult -> Query.Profile
    toProfile UserUseCase.FollowUserResult {..} =
      Query.Profile
        { profileUsername = followUserResultUsername,
          profileBio = followUserResultBio,
          profileImage = followUserResultImage,
          profileFollowing = followUserResultFollowing
        }

----------------------------------------------------------------------------------------------------
-- Unfollow User

unfollow :: (MonadIO m, UserRepository m, TokenGateway m, TxManager m) => ActionT ErrorResponse m ()
unfollow = do
  withToken $ \token -> do
    username <- param "username"
    result <- lift $ UserUseCase.unfollowUser $ toCommand (unToken token) username
    case result of
      Right result' -> json $ object ["profile" .= toProfile result']
      Left err -> raise $ invalid $ show err
  where
    toCommand :: Text -> Text -> UserUseCase.UnfollowUserCommand
    toCommand token username =
      UserUseCase.UnfollowUserCommand
        { unfollowUserCommandToken = token,
          unfollowUserCommandUsername = username
        }
    toProfile :: UserUseCase.UnfollowUserResult -> Query.Profile
    toProfile UserUseCase.UnfollowUserResult {..} =
      Query.Profile
        { profileUsername = unfollowUserResultUsername,
          profileBio = unfollowUserResultBio,
          profileImage = unfollowUserResultImage,
          profileFollowing = unfollowUserResultFollowing
        }
