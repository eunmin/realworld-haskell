{-# LANGUAGE NoFieldSelectors #-}

module RealWorld.Domain.Command.User.UseCase where

import Control.Error (throwE)
import Control.Error.Util ((!?), (??))
import Data.Time (getCurrentTime)
import Data.ULID (getULID)
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway)
import RealWorld.Domain.Adapter.Gateway.PasswordGateway qualified as PasswordGateway
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import RealWorld.Domain.Adapter.Gateway.TokenGateway qualified as TokenGateway
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager (withTx))
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Adapter.Repository.UserRepository qualified as UserRepo
import RealWorld.Domain.Command.User.Entity.User ()
import RealWorld.Domain.Command.User.Entity.User qualified as User
import RealWorld.Domain.Command.User.Value (
  Bio (unBio),
  Email (unEmail),
  Image (unImage),
  Token (Token, unToken),
  Username (unUsername),
  mkBio,
  mkEmail,
  mkImage,
  mkPassword,
  mkUsername,
  tokeExpiresInSec,
 )
import RealWorld.Domain.Util.BoundedText (unBoundedText)
import RealWorld.Domain.Util.Maybe (justToNothing)
import Relude hiding ((??))
import Prelude hiding ((??))

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
  deriving stock (Eq, Generic)

registration ::
  (MonadIO m, UserRepository m, TokenGateway m, PasswordGateway m, TxManager m) =>
  RegistrationCommand ->
  m (Either RegistrationError RegistrationResult)
registration command = runExceptT $ do
  userId <- liftIO getULID
  createdAt <- liftIO getCurrentTime
  username <- mkUsername command.username ?? RegistrationErrorInvalidUsername
  email <- mkEmail command.email ?? RegistrationErrorInvalidEmail
  password <- mkPassword command.password ?? RegistrationErrorInvalidPassword
  hashedPassword <- PasswordGateway.hashPassword password !? RegistrationErrorInvalidPassword
  _ <- withTx $ do
    (justToNothing <$> UserRepo.findByUsername username) !? RegistrationErrorUsernameAlreadyExists
    (justToNothing <$> UserRepo.findByEmail email) !? RegistrationErrorEmailAlreadyExists
    let user = User.mkUser userId username email hashedPassword createdAt
    lift $ UserRepo.save user
  token <- lift $ TokenGateway.generate userId tokeExpiresInSec
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
  deriving stock (Eq, Generic)

authentication ::
  (MonadIO m, UserRepository m, PasswordGateway m, TokenGateway m) =>
  AuthenticationCommand ->
  m (Either AuthenticationError AuthenticationResult)
authentication command = runExceptT $ do
  email <- mkEmail command.email ?? AuthenticationErrorInvalidEmail
  password <- mkPassword command.password ?? AuthenticationErrorInvalidPassword
  user <- UserRepo.findByEmail email !? AuthenticationErrorUserNotFound
  whenM (lift $ PasswordGateway.isValidPassword user.hashedPassword password) $
    throwE AuthenticationErrorInvalidPassword
  token <- lift $ TokenGateway.generate user.userId tokeExpiresInSec
  pure $
    AuthenticationResult
      { token = unToken token
      , username = unBoundedText . unUsername $ user.username
      , bio = unBio user.bio
      , image = unImage <$> user.image
      }

----------------------------------------------------------------------------------------------------
-- Update User

data UpdateUserCommand = UpdateUserCommand
  { token :: Text
  , username :: Maybe Text
  , email :: Maybe Text
  , password :: Maybe Text
  , bio :: Maybe Text
  , image :: Maybe (Maybe Text)
  }
  deriving stock (Show, Eq, Generic)

data UpdateUserResult = UpdateUserResult
  { token :: Text
  , username :: Text
  , email :: Text
  , bio :: Text
  , image :: Maybe Text
  }
  deriving stock (Show, Eq)

data UpdateUserError
  = UpdateUserErrorInvalidToken
  | UpdateUserErrorInvalidUsername
  | UpdateUserErrorInvalidEmail
  | UpdateUserErrorInvalidPassword
  | UpdateUserErrorUserNotFound
  | UpdateUserErrorUsernameAlreadyExists
  deriving stock (Show, Eq, Generic)

updateUser ::
  (MonadIO m, UserRepository m, PasswordGateway m, TokenGateway m, TxManager m) =>
  UpdateUserCommand ->
  m (Either UpdateUserError UpdateUserResult)
updateUser command = runExceptT $ do
  username <- traverse mkUsername command.username ?? UpdateUserErrorInvalidUsername
  email <- traverse mkEmail command.email ?? UpdateUserErrorInvalidEmail
  password <- traverse mkPassword command.password ?? UpdateUserErrorInvalidPassword
  userId <- TokenGateway.verify (Token command.token) !? UpdateUserErrorInvalidToken
  let bio = mkBio <$> command.bio
  let image = mkImage <$> command.image
  hashedPassword <- case password of
    Nothing -> pure Nothing
    Just password' ->
      Just
        <$> PasswordGateway.hashPassword password'
          !? UpdateUserErrorInvalidPassword
  user <- withTx $ do
    user <- UserRepo.findById userId !? UpdateUserErrorUserNotFound
    whenJust username $ \username' ->
      when (username' /= user.username) $ do
        (justToNothing <$> UserRepo.findByUsername username')
          !? UpdateUserErrorUsernameAlreadyExists
    let user' = User.update user username email hashedPassword bio image
    _ <- lift $ UserRepo.save user'
    pure user'
  pure $
    UpdateUserResult
      { token = command.token
      , username = unBoundedText . unUsername $ user.username
      , email = unEmail user.email
      , bio = unBio user.bio
      , image = unImage <$> user.image
      }

----------------------------------------------------------------------------------------------------
-- Follow User

data FollowUserCommand = FollowUserCommand
  { token :: Text
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
  = FollowUserErrorInvalidToken
  | FollowUserErrorUserNotFound
  | FollowUserErrorCantFollowSelf
  | FollowUserErrorAlreadyFollowing
  | FollowUserErrorInvalidUsername
  deriving stock (Eq, Generic)

followUser ::
  (MonadIO m, UserRepository m, TokenGateway m, TxManager m) =>
  FollowUserCommand ->
  m (Either FollowUserError FollowUserResult)
followUser command = runExceptT $ do
  followerId <- TokenGateway.verify (Token command.token) !? FollowUserErrorInvalidToken
  username <- mkUsername command.username ?? FollowUserErrorInvalidUsername
  user <- withTx $ do
    followee <- UserRepo.findByUsername username !? FollowUserErrorUserNotFound
    isAlreadyFollow <- lift $ UserRepo.hasFollowing followerId followee.userId
    when isAlreadyFollow $ throwE FollowUserErrorAlreadyFollowing
    when (followerId == followee.userId) $ throwE FollowUserErrorCantFollowSelf
    _ <- lift $ UserRepo.follow followerId followee.userId
    pure followee
  pure $
    FollowUserResult
      { username = unBoundedText . unUsername $ user.username
      , bio = unBio user.bio
      , image = unImage <$> user.image
      , following = True
      }

----------------------------------------------------------------------------------------------------
-- Unfollow User

data UnfollowUserCommand = UnfollowUserCommand
  { token :: Text
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
  = UnfollowUserErrorInvalidToken
  | UnfollowUserErrorInvalidUsername
  | UnfollowUserErrorUserNotFound
  deriving stock (Show, Eq, Generic)

unfollowUser ::
  (MonadIO m, UserRepository m, TokenGateway m, TxManager m) =>
  UnfollowUserCommand ->
  m (Either UnfollowUserError UnfollowUserResult)
unfollowUser command = runExceptT $ do
  followerId <- TokenGateway.verify (Token command.token) !? UnfollowUserErrorInvalidToken
  username <- mkUsername command.username ?? UnfollowUserErrorInvalidUsername
  user <- withTx $ do
    followee <- UserRepo.findByUsername username !? UnfollowUserErrorUserNotFound
    _ <- lift $ UserRepo.unfollow followerId followee.userId
    pure followee
  pure $
    UnfollowUserResult
      { username = unBoundedText . unUsername $ user.username
      , bio = unBio user.bio
      , image = unImage <$> user.image
      , following = False
      }
