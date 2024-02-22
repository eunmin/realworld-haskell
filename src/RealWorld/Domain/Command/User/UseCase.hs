{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module RealWorld.Domain.Command.User.UseCase where

import Control.Error (throwE)
import Control.Error.Util ((!?), (??))
import Data.Time (getCurrentTime)
import Data.ULID (getULID)
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway)
import qualified RealWorld.Domain.Adapter.Gateway.PasswordGateway as PasswordGateway
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import qualified RealWorld.Domain.Adapter.Gateway.TokenGateway as TokenGateway
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager (withTx))
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import qualified RealWorld.Domain.Adapter.Repository.UserRepository as UserRepo
import RealWorld.Domain.Command.User.Entity.User
  ( User (userBio, userEmail, userHashedPassword, userId, userImage, userUsername),
  )
import qualified RealWorld.Domain.Command.User.Entity.User as User
import RealWorld.Domain.Command.User.Value
  ( Bio (unBio),
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

----------------------------------------------------------------------------------------------------
-- Registration

data RegistrationCommand = RegistrationCommand
  { registrationCommandUsername :: Text,
    registrationCommandEmail :: Text,
    registrationCommandPassword :: Text
  }
  deriving (Show, Eq, Generic)

data RegistrationResult = RegistrationResult
  { registrationResultToken :: Text
  }
  deriving (Show, Eq)

data RegistrationError
  = RegistrationErrorUsernameAlreadyExists
  | RegistrationErrorEmailAlreadyExists
  | RegistrationErrorInvalidPassword
  | RegistrationErrorInvalidUsername
  | RegistrationErrorInvalidEmail
  deriving (Show, Eq)

registration ::
  (MonadIO m, UserRepository m, TokenGateway m, PasswordGateway m, TxManager m) =>
  RegistrationCommand ->
  m (Either RegistrationError RegistrationResult)
registration RegistrationCommand {..} = runExceptT $ do
  userId <- liftIO getULID
  createdAt <- liftIO getCurrentTime
  username <- mkUsername registrationCommandUsername ?? RegistrationErrorInvalidUsername
  email <- mkEmail registrationCommandEmail ?? RegistrationErrorInvalidUsername
  password <- mkPassword registrationCommandPassword ?? RegistrationErrorInvalidPassword
  hashedPassword <- PasswordGateway.hashPassword password !? RegistrationErrorInvalidPassword
  withTx $ do
    (justToNothing <$> UserRepo.findByUsername username) !? RegistrationErrorUsernameAlreadyExists
    (justToNothing <$> UserRepo.findByEmail email) !? RegistrationErrorEmailAlreadyExists
    let user = User.mkUser userId username email hashedPassword createdAt
    lift $ UserRepo.save user
  token <- lift $ TokenGateway.generate userId tokeExpiresInSec
  pure $ RegistrationResult $ unToken token

----------------------------------------------------------------------------------------------------
-- Authentication

data AuthenticationCommand = AuthenticationCommand
  { authenticationCommandEmail :: Text,
    authenticationCommandPassword :: Text
  }
  deriving (Show, Eq, Generic)

data AuthenticationResult = AuthenticationResult
  { authenticationResultToken :: Text,
    authenticationResultUsername :: Text,
    authenticationResultBio :: Text,
    authenticationResultImage :: Maybe Text
  }
  deriving (Show, Eq)

data AuthenticationError
  = AuthenticationErrorUserNotFound
  | AuthenticationErrorInvalidPassword
  | AuthenticationErrorInvalidEmail
  deriving (Show, Eq)

authentication ::
  (MonadIO m, UserRepository m, PasswordGateway m, TokenGateway m) =>
  AuthenticationCommand ->
  m (Either AuthenticationError AuthenticationResult)
authentication AuthenticationCommand {..} = runExceptT $ do
  email <- mkEmail authenticationCommandEmail ?? AuthenticationErrorInvalidEmail
  password <- mkPassword authenticationCommandPassword ?? AuthenticationErrorInvalidPassword
  user <- UserRepo.findByEmail email !? AuthenticationErrorUserNotFound
  whenM (lift $ PasswordGateway.isValidPassword (userHashedPassword user) password)
    $ throwE AuthenticationErrorInvalidPassword
  token <- lift $ TokenGateway.generate (userId user) tokeExpiresInSec
  pure
    $ AuthenticationResult
      { authenticationResultToken = unToken token,
        authenticationResultUsername = unBoundedText . unUsername . userUsername $ user,
        authenticationResultBio = unBio $ userBio user,
        authenticationResultImage = unImage <$> userImage user
      }

----------------------------------------------------------------------------------------------------
-- Update User

data UpdateUserCommand = UpdateUserCommand
  { updateUserCommandToken :: Text,
    updateUserCommandUserName :: Maybe Text,
    updateUserCommandEmail :: Maybe Text,
    updateUserCommandPassword :: Maybe Text,
    updateUserCommandBio :: Maybe Text,
    updateUserCommandImage :: Maybe (Maybe Text)
  }
  deriving (Show, Eq, Generic)

data UpdateUserResult = UpdateUserResult
  { updateUserResultToken :: Text,
    updateUserResultUsername :: Text,
    updateUserResultEmail :: Text,
    updateUserResultBio :: Text,
    updateUserResultImage :: Maybe Text
  }
  deriving (Show, Eq)

data UpdateUserError
  = UpdateUserErrorInvalidToken
  | UpdateUserErrorInvalidUsername
  | UpdateUserErrorInvalidEmail
  | UpdateUserErrorInvalidPassword
  | UpdateUserErrorUserNotFound
  | UpdateUserErrorUsernameAlreadyExists
  deriving (Show, Eq)

updateUser ::
  (MonadIO m, UserRepository m, PasswordGateway m, TokenGateway m, TxManager m) =>
  UpdateUserCommand ->
  m (Either UpdateUserError UpdateUserResult)
updateUser UpdateUserCommand {..} = runExceptT $ do
  username <- traverse mkUsername updateUserCommandUserName ?? UpdateUserErrorInvalidUsername
  email <- traverse mkEmail updateUserCommandEmail ?? UpdateUserErrorInvalidEmail
  password <- traverse mkPassword updateUserCommandPassword ?? UpdateUserErrorInvalidPassword
  userId <- TokenGateway.verify (Token updateUserCommandToken) !? UpdateUserErrorInvalidToken
  let bio = mkBio <$> updateUserCommandBio
  let image = mkImage <$> updateUserCommandImage
  hashedPassword <- case password of
    Nothing -> pure Nothing
    Just password' ->
      Just
        <$> PasswordGateway.hashPassword password'
        !? UpdateUserErrorInvalidPassword
  user <- withTx $ do
    user <- UserRepo.findById userId !? UpdateUserErrorUserNotFound
    whenJust username $ \username' ->
      when (username' /= userUsername user) $ do
        (justToNothing <$> UserRepo.findByUsername username')
          !? UpdateUserErrorUsernameAlreadyExists
    let user' = User.update user username email hashedPassword bio image
    lift $ UserRepo.save user'
    pure user'
  pure
    $ UpdateUserResult
      { updateUserResultToken = updateUserCommandToken,
        updateUserResultUsername = unBoundedText . unUsername . userUsername $ user,
        updateUserResultEmail = unEmail $ userEmail user,
        updateUserResultBio = unBio $ userBio user,
        updateUserResultImage = unImage <$> userImage user
      }

----------------------------------------------------------------------------------------------------
-- Follow User

data FollowUserCommand = FollowUserCommand
  { followUserCommandToken :: Text,
    followUserCommandUsername :: Text
  }
  deriving (Show, Eq, Generic)

data FollowUserResult = FollowUserResult
  { followUserResultUsername :: Text,
    followUserResultBio :: Text,
    followUserResultImage :: Maybe Text,
    followUserResultFollowing :: Bool
  }
  deriving (Show, Eq)

data FollowUserError
  = FollowUserErrorInvalidToken
  | FollowUserErrorUserNotFound
  | FollowUserErrorCantFollowSelf
  | FollowUserErrorAlreadyFollowing
  | FollowUserErrorInvalidUsername
  deriving (Show, Eq)

followUser ::
  (MonadIO m, UserRepository m, TokenGateway m, TxManager m) =>
  FollowUserCommand ->
  m (Either FollowUserError FollowUserResult)
followUser FollowUserCommand {..} = runExceptT $ do
  followerId <- TokenGateway.verify (Token followUserCommandToken) !? FollowUserErrorInvalidToken
  username <- mkUsername followUserCommandUsername ?? FollowUserErrorInvalidUsername
  user <- withTx $ do
    followee <- UserRepo.findByUsername username !? FollowUserErrorUserNotFound
    isAlreadyFollow <- lift $ UserRepo.hasFollowing followerId (userId followee)
    when isAlreadyFollow $ throwE FollowUserErrorAlreadyFollowing
    when (followerId == userId followee) $ throwE FollowUserErrorCantFollowSelf
    _ <- lift $ UserRepo.follow followerId (userId followee)
    pure followee
  pure
    $ FollowUserResult
      { followUserResultUsername = unBoundedText . unUsername . userUsername $ user,
        followUserResultBio = unBio $ userBio user,
        followUserResultImage = unImage <$> userImage user,
        followUserResultFollowing = True
      }

----------------------------------------------------------------------------------------------------
-- Unfollow User

data UnfollowUserCommand = UnfollowUserCommand
  { unfollowUserCommandToken :: Text,
    unfollowUserCommandUsername :: Text
  }
  deriving (Show, Eq, Generic)

data UnfollowUserResult = UnfollowUserResult
  { unfollowUserResultUsername :: Text,
    unfollowUserResultBio :: Text,
    unfollowUserResultImage :: Maybe Text,
    unfollowUserResultFollowing :: Bool
  }
  deriving (Show, Eq)

data UnfollowUserError
  = UnfollowUserErrorInvalidToken
  | UnfollowUserErrorInvalidUsername
  | UnfollowUserErrorUserNotFound
  deriving (Show, Eq)

unfollowUser ::
  (MonadIO m, UserRepository m, TokenGateway m, TxManager m) =>
  UnfollowUserCommand ->
  m (Either UnfollowUserError UnfollowUserResult)
unfollowUser UnfollowUserCommand {..} = runExceptT $ do
  followerId <- TokenGateway.verify (Token unfollowUserCommandToken) !? UnfollowUserErrorInvalidToken
  username <- mkUsername unfollowUserCommandUsername ?? UnfollowUserErrorInvalidUsername
  user <- withTx $ do
    followee <- UserRepo.findByUsername username !? UnfollowUserErrorUserNotFound
    _ <- lift $ UserRepo.unfollow followerId (userId followee)
    pure followee
  pure
    $ UnfollowUserResult
      { unfollowUserResultUsername = unBoundedText . unUsername . userUsername $ user,
        unfollowUserResultBio = unBio $ userBio user,
        unfollowUserResultImage = unImage <$> userImage user,
        unfollowUserResultFollowing = False
      }