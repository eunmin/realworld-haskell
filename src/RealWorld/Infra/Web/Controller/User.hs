{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module RealWorld.Infra.Web.Controller.User where

import Control.Lens ((.~))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON,
  genericParseJSON,
  withObject,
  (.:!),
 )
import Data.Aeson.Casing (aesonDrop, camelCase)
import Data.Generics.Labels ()
import Katip
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import RealWorld.Domain.Adapter.Gateway.TokenGateway qualified as TokenGateway
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.User.UseCase qualified as UserUseCase
import RealWorld.Domain.Command.User.Value (Token (Token))
import RealWorld.Domain.Query.Data qualified as Query
import RealWorld.Domain.Query.QueryService (QueryService)
import RealWorld.Domain.Query.QueryService qualified as QueryService
import RealWorld.Infra.Converter.Aeson ()
import RealWorld.Infra.Web.ErrorResponse (
  invalid,
  notFound,
  unauthorized,
 )
import RealWorld.Infra.Web.Errors ()
import RealWorld.Infra.Web.Util (withOptionalToken, withRequiredToken, (!?))
import Web.Scotty.Trans (ActionT, json, jsonData, pathParam, throw)

data UserWrapper a = UserWrapper
  { user :: a
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ProfileWrapper a = ProfileWrapper
  { profile :: a
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

----------------------------------------------------------------------------------------------------
-- Registration

data RegistrationInput = RegistrationInput
  { email :: Text
  , username :: Text
  , password :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON RegistrationInput where
  parseJSON = genericParseJSON $ aesonDrop 17 camelCase

registration ::
  (KatipContext m, UserRepository m, TokenGateway m, PasswordGateway m, TxManager m) =>
  ActionT m ()
registration = do
  UserWrapper input <- jsonData
  result <- lift $ UserUseCase.registration $ toCommand input
  case result of
    Right result' -> json $ UserWrapper $ toUser input result'
    Left err -> do
      lift $ katipAddContext (sl "error" err) $ do
        $(logTM) ErrorS "registration error"
      throw $ invalid $ show err
  where
    toCommand :: RegistrationInput -> UserUseCase.RegistrationCommand
    toCommand input =
      UserUseCase.RegistrationCommand
        { username = input.username
        , email = input.email
        , password = input.password
        }
    toUser input (UserUseCase.RegistrationResult token) =
      Query.User
        { email = input.email
        , token = token
        , username = input.username
        , bio = ""
        , image = Nothing
        }

----------------------------------------------------------------------------------------------------
-- Authentication

data AuthenticationInput = AuthenticationInput
  { email :: Text
  , password :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON AuthenticationInput where
  parseJSON = genericParseJSON $ aesonDrop 19 camelCase

authentication ::
  (KatipContext m, UserRepository m, TokenGateway m, PasswordGateway m) =>
  ActionT m ()
authentication = do
  UserWrapper input <- jsonData
  result <- lift $ UserUseCase.authentication $ toCommand input
  case result of
    Right result' -> json $ UserWrapper $ toUser input result'
    Left err -> do
      lift $ katipAddContext (sl "error" err) $ do
        $(logTM) ErrorS "authentication error"
      throw $ invalid $ show err
  where
    toCommand :: AuthenticationInput -> UserUseCase.AuthenticationCommand
    toCommand input =
      UserUseCase.AuthenticationCommand
        { email = input.email
        , password = input.password
        }
    toUser input result =
      Query.User
        { email = input.email
        , token = result.token
        , username = result.username
        , bio = result.bio
        , image = result.image
        }

----------------------------------------------------------------------------------------------------
-- Get Current User

getCurrentUser :: (MonadIO m, QueryService m, TokenGateway m) => ActionT m ()
getCurrentUser = do
  withRequiredToken $ \token -> do
    userId <- TokenGateway.verify (Token token) !? unauthorized "Unauthorized"
    let params = Query.GetCurrentUserParams $ show userId
    user <- QueryService.getCurrentUser params !? notFound "User not found"
    json $ UserWrapper $ user & #token .~ token

----------------------------------------------------------------------------------------------------
-- Update User

data UpdateUserInput = UpdateUserInput
  { email :: Maybe Text
  , username :: Maybe Text
  , password :: Maybe Text
  , bio :: Maybe Text
  , image :: Maybe (Maybe Text)
  }
  deriving stock (Show, Generic)

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
  (KatipContext m, UserRepository m, TokenGateway m, PasswordGateway m, TxManager m) =>
  ActionT m ()
updateUser = do
  withRequiredToken $ \token -> do
    UserWrapper input <- jsonData
    result <- lift $ UserUseCase.updateUser $ toCommand input token
    case result of
      Right result' -> json $ UserWrapper $ toUser result'
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "updateUser error"
        throw $ invalid $ show err
  where
    toCommand :: UpdateUserInput -> Text -> UserUseCase.UpdateUserCommand
    toCommand input token =
      UserUseCase.UpdateUserCommand
        { token = token
        , username = input.username
        , email = input.email
        , password = input.password
        , bio = input.bio
        , image = input.image
        }
    toUser :: UserUseCase.UpdateUserResult -> Query.User
    toUser result =
      Query.User
        { email = result.email
        , token = result.token
        , username = result.username
        , bio = result.bio
        , image = result.image
        }

----------------------------------------------------------------------------------------------------
-- Get Profile

getProfile :: (MonadIO m, QueryService m, TokenGateway m) => ActionT m ()
getProfile = do
  withOptionalToken $ \token -> do
    userId <- case Token <$> token of
      Just token' -> lift $ TokenGateway.verify token'
      Nothing -> pure Nothing
    username <- pathParam "username"
    let params = Query.GetProfileParams (Just $ show userId) username
    profile <- QueryService.getProfile params !? notFound "Profile not found"
    json $ ProfileWrapper profile

----------------------------------------------------------------------------------------------------
-- Follow User

follow :: (KatipContext m, UserRepository m, TokenGateway m, TxManager m) => ActionT m ()
follow = do
  withRequiredToken $ \token -> do
    username <- pathParam "username"
    result <- lift $ UserUseCase.followUser $ toCommand token username
    case result of
      Right result' -> json $ ProfileWrapper $ toProfile result'
      Left err -> do
        lift $ katipAddContext (sl "error" err <> sl "username" username) $ do
          $(logTM) ErrorS "follow error"
        throw $ invalid $ show err
  where
    toCommand :: Text -> Text -> UserUseCase.FollowUserCommand
    toCommand token username =
      UserUseCase.FollowUserCommand
        { token = token
        , username = username
        }
    toProfile result =
      Query.Profile
        { username = result.username
        , bio = result.bio
        , image = result.image
        , following = result.following
        }

----------------------------------------------------------------------------------------------------
-- Unfollow User

unfollow :: (KatipContext m, UserRepository m, TokenGateway m, TxManager m) => ActionT m ()
unfollow = do
  withRequiredToken $ \token -> do
    username <- pathParam "username"
    result <- lift $ UserUseCase.unfollowUser $ toCommand token username
    case result of
      Right result' -> json $ ProfileWrapper $ toProfile result'
      Left err -> do
        lift $ katipAddContext (sl "error" err <> sl "username" username) $ do
          $(logTM) ErrorS "unfollow error"
        throw $ invalid $ show err
  where
    toCommand :: Text -> Text -> UserUseCase.UnfollowUserCommand
    toCommand token username =
      UserUseCase.UnfollowUserCommand
        { token = token
        , username = username
        }
    toProfile result =
      Query.Profile
        { username = result.username
        , bio = result.bio
        , image = result.image
        , following = result.following
        }
