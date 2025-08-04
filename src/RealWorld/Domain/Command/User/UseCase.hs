{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module RealWorld.Domain.Command.User.UseCase where

import Data.Aeson (ToJSON)
import Data.Time (getCurrentTime)
import Data.ULID (getULID)
import Effectful (Eff, IOE, type (:>))
import Effectful.Error.Dynamic (runErrorNoCallStack, throwError)
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway)
import qualified RealWorld.Domain.Adapter.Gateway.PasswordGateway as PasswordGateway
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import qualified RealWorld.Domain.Adapter.Gateway.TokenGateway as TokenGateway
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager, withTx)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import qualified RealWorld.Domain.Adapter.Repository.UserRepository as UserRepo
import RealWorld.Domain.Command.User.Entity.User ()
import qualified RealWorld.Domain.Command.User.Entity.User as User
import RealWorld.Domain.Command.User.Value (
  Bio (unBio),
  Email (unEmail),
  Image (unImage),
  Token (unToken),
  Username (unUsername),
  mkBio,
  mkEmail,
  mkImage,
  mkPassword,
  mkUsername,
  tokeExpiresInSec,
 )
import RealWorld.Domain.Util.BoundedText (unBoundedText)
import Relude

----------------------------------------------------------------------------------------------------
-- Registration

data RegistrationCommand = RegistrationCommand
  { username :: Text
  , email :: Text
  , password :: Text
  }
  deriving stock (Show, Eq, Generic)

data RegistrationResult = RegistrationResult
  { token :: Text
  }
  deriving stock (Show, Eq)

data RegistrationError
  = RegistrationErrorUsernameAlreadyExists
  | RegistrationErrorEmailAlreadyExists
  | RegistrationErrorInvalidPassword
  | RegistrationErrorInvalidUsername
  | RegistrationErrorInvalidEmail
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

registration ::
  (IOE :> es, UserRepository :> es, TokenGateway :> es, PasswordGateway :> es, TxManager :> es) =>
  RegistrationCommand ->
  Eff es (Either RegistrationError RegistrationResult)
registration command = runErrorNoCallStack $ do
  userId <- liftIO getULID
  createdAt <- liftIO getCurrentTime
  username <- mkUsername command.username `whenNothing` throwError RegistrationErrorInvalidUsername
  email <- mkEmail command.email `whenNothing` throwError RegistrationErrorInvalidEmail
  password <- mkPassword command.password `whenNothing` throwError RegistrationErrorInvalidPassword
  hashedPassword <- PasswordGateway.hashPassword password `whenNothingM` throwError RegistrationErrorInvalidPassword
  _ <- withTx $ do
    UserRepo.findByUsername username `whenJustM` const (throwError RegistrationErrorUsernameAlreadyExists)
    UserRepo.findByEmail email `whenJustM` const (throwError RegistrationErrorEmailAlreadyExists)
    let user = User.mkUser userId username email hashedPassword createdAt
    UserRepo.save user
  token <- TokenGateway.generate userId tokeExpiresInSec
  pure $ RegistrationResult $ unToken token

----------------------------------------------------------------------------------------------------
-- Authentication

data AuthenticationCommand = AuthenticationCommand
  { email :: Text
  , password :: Text
  }
  deriving stock (Show, Eq, Generic)

data AuthenticationResult = AuthenticationResult
  { token :: Text
  , username :: Text
  , bio :: Text
  , image :: Maybe Text
  }
  deriving stock (Show, Eq)

data AuthenticationError
  = AuthenticationErrorUserNotFound
  | AuthenticationErrorInvalidPassword
  | AuthenticationErrorInvalidEmail
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

authentication ::
  (UserRepository :> es, PasswordGateway :> es, TokenGateway :> es) =>
  AuthenticationCommand ->
  Eff es (Either AuthenticationError AuthenticationResult)
authentication command = runErrorNoCallStack $ do
  email <- mkEmail command.email `whenNothing` throwError AuthenticationErrorInvalidEmail
  password <- mkPassword command.password `whenNothing` throwError AuthenticationErrorInvalidPassword
  user <- UserRepo.findByEmail email `whenNothingM` throwError AuthenticationErrorUserNotFound
  whenM (PasswordGateway.isValidPassword user.hashedPassword password) $
    throwError AuthenticationErrorInvalidPassword
  token <- TokenGateway.generate user.userId tokeExpiresInSec
  pure $
    AuthenticationResult
      { token = token.unToken
      , username = user.username.unUsername.unBoundedText
      , bio = user.bio.unBio
      , image = unImage <$> user.image
      }

----------------------------------------------------------------------------------------------------
-- Update User

data UpdateUserCommand = UpdateUserCommand
  { userId :: Text
  , username :: Maybe Text
  , email :: Maybe Text
  , password :: Maybe Text
  , bio :: Maybe Text
  , image :: Maybe (Maybe Text)
  }
  deriving stock (Show, Eq, Generic)

data UpdateUserResult = UpdateUserResult
  { userId :: Text
  , username :: Text
  , email :: Text
  , bio :: Text
  , image :: Maybe Text
  }
  deriving stock (Show, Eq)

data UpdateUserError
  = UpdateUserErrorInvalidUserId
  | UpdateUserErrorInvalidUsername
  | UpdateUserErrorInvalidEmail
  | UpdateUserErrorInvalidPassword
  | UpdateUserErrorUserNotFound
  | UpdateUserErrorUsernameAlreadyExists
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

updateUser ::
  (UserRepository :> es, PasswordGateway :> es, TxManager :> es) =>
  UpdateUserCommand ->
  Eff es (Either UpdateUserError UpdateUserResult)
updateUser command = runErrorNoCallStack $ do
  username <- traverse mkUsername command.username `whenNothing` throwError UpdateUserErrorInvalidUsername
  email <- traverse mkEmail command.email `whenNothing` throwError UpdateUserErrorInvalidEmail
  password <- traverse mkPassword command.password `whenNothing` throwError UpdateUserErrorInvalidPassword
  userId <- readMaybe (toString command.userId) `whenNothing` throwError UpdateUserErrorInvalidUserId
  let bio = mkBio <$> command.bio
  let image = mkImage <$> command.image
  hashedPassword <- case password of
    Nothing -> pure Nothing
    Just password' ->
      Just
        <$> PasswordGateway.hashPassword password'
          `whenNothingM` throwError UpdateUserErrorInvalidPassword
  user <- withTx $ do
    user <- UserRepo.findById userId `whenNothingM` throwError UpdateUserErrorUserNotFound
    whenJust username $ \username' ->
      when (username' /= user.username) $ do
        UserRepo.findByUsername username' `whenJustM` const (throwError UpdateUserErrorUsernameAlreadyExists)
    let user' = User.update user username email hashedPassword bio image
    _ <- UserRepo.save user'
    pure user'
  pure $
    UpdateUserResult
      { userId = show userId
      , username = user.username.unUsername.unBoundedText
      , email = user.email.unEmail
      , bio = user.bio.unBio
      , image = unImage <$> user.image
      }

----------------------------------------------------------------------------------------------------
-- Follow User

data FollowUserCommand = FollowUserCommand
  { userId :: Text
  , username :: Text
  }
  deriving stock (Show, Eq, Generic)

data FollowUserResult = FollowUserResult
  { username :: Text
  , bio :: Text
  , image :: Maybe Text
  , following :: Bool
  }
  deriving stock (Show, Eq)

data FollowUserError
  = FollowUserErrorInvalidUserId
  | FollowUserErrorUserNotFound
  | FollowUserErrorCantFollowSelf
  | FollowUserErrorAlreadyFollowing
  | FollowUserErrorInvalidUsername
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

followUser ::
  (UserRepository :> es, TxManager :> es) =>
  FollowUserCommand ->
  Eff es (Either FollowUserError FollowUserResult)
followUser command = runErrorNoCallStack $ do
  followerId <- readMaybe (toString command.userId) `whenNothing` throwError FollowUserErrorInvalidUserId
  username <- mkUsername command.username `whenNothing` throwError FollowUserErrorInvalidUsername
  user <- withTx $ do
    followee <- UserRepo.findByUsername username `whenNothingM` throwError FollowUserErrorUserNotFound
    isAlreadyFollow <- UserRepo.hasFollowing followerId followee.userId
    when isAlreadyFollow $ throwError FollowUserErrorAlreadyFollowing
    when (followerId == followee.userId) $ throwError FollowUserErrorCantFollowSelf
    _ <- UserRepo.follow followerId followee.userId
    pure followee
  pure $
    FollowUserResult
      { username = user.username.unUsername.unBoundedText
      , bio = user.bio.unBio
      , image = unImage <$> user.image
      , following = True
      }

----------------------------------------------------------------------------------------------------
-- Unfollow User

data UnfollowUserCommand = UnfollowUserCommand
  { userId :: Text
  , username :: Text
  }
  deriving stock (Show, Eq, Generic)

data UnfollowUserResult = UnfollowUserResult
  { username :: Text
  , bio :: Text
  , image :: Maybe Text
  , following :: Bool
  }
  deriving stock (Show, Eq)

data UnfollowUserError
  = UnfollowUserErrorInvalidUserId
  | UnfollowUserErrorInvalidUsername
  | UnfollowUserErrorUserNotFound
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

unfollowUser ::
  (UserRepository :> es, TxManager :> es) =>
  UnfollowUserCommand ->
  Eff es (Either UnfollowUserError UnfollowUserResult)
unfollowUser command = runErrorNoCallStack $ do
  followerId <- readMaybe (toString command.userId) `whenNothing` throwError UnfollowUserErrorInvalidUserId
  username <- mkUsername command.username `whenNothing` throwError UnfollowUserErrorInvalidUsername
  user <- withTx $ do
    followee <- UserRepo.findByUsername username `whenNothingM` throwError UnfollowUserErrorUserNotFound
    _ <- UserRepo.unfollow followerId followee.userId
    pure followee
  pure $
    UnfollowUserResult
      { username = user.username.unUsername.unBoundedText
      , bio = unBio user.bio
      , image = unImage <$> user.image
      , following = False
      }
