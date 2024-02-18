module RealWorld.Domain.User.UseCase where

import Control.Error (throwE)
import Control.Error.Util ((!?))
import Data.Time (getCurrentTime)
import Data.ULID (getULID)
import RealWorld.Domain.Repo (Tx (withTx))
import RealWorld.Domain.User.Gateway.Password (PasswordGateway)
import qualified RealWorld.Domain.User.Gateway.Password as PasswordGateway
import RealWorld.Domain.User.Gateway.Token (TokenGateway)
import qualified RealWorld.Domain.User.Gateway.Token as TokenGateway
import RealWorld.Domain.User.Repo (UserRepository)
import qualified RealWorld.Domain.User.Repo as UserRepo
import RealWorld.Domain.User.Types
  ( AuthorizedUser,
    Bio,
    Email,
    Image,
    Password,
    Profile,
    Token,
    User (userHashedPassword, userId, userUsername),
    Username,
    mkBio,
    mkEmail,
    mkImage,
    mkPassword,
    mkUsername,
  )
import qualified RealWorld.Domain.User.Types as User
import RealWorld.Util.Maybe (justToNothing)
import Relude

----------------------------------------------------------------------------------------------------
-- Registration

data RegistrationCommand = RegistrationCommand
  { registrationCommandUserName :: Username,
    registrationCommandEmail :: Email,
    registrationCommandPassword :: Password
  }
  deriving (Show, Eq, Generic)

mkRegistrationCommand :: Text -> Text -> Text -> Maybe RegistrationCommand
mkRegistrationCommand username email password = do
  RegistrationCommand
    <$> mkUsername username
    <*> mkEmail email
    <*> mkPassword password

data RegistrationError
  = RegistrationErrorUsernameAlreadyExists
  | RegistrationErrorEmailAlreadyExists
  | RegistrationErrorInvalidPassword
  deriving (Show, Eq)

registration ::
  (MonadIO m, UserRepository m, TokenGateway m, PasswordGateway m, Tx m) =>
  RegistrationCommand ->
  m (Either RegistrationError AuthorizedUser)
registration (RegistrationCommand username email password) = runExceptT $ do
  userId <- liftIO getULID
  createdAt <- liftIO getCurrentTime
  hashedPassword <- PasswordGateway.hashPassword password !? RegistrationErrorInvalidPassword
  user <- withTx $ do
    (justToNothing <$> UserRepo.findByUsername username) !? RegistrationErrorUsernameAlreadyExists
    (justToNothing <$> UserRepo.findByEmail email) !? RegistrationErrorEmailAlreadyExists
    let user = User.mkUser userId username email hashedPassword createdAt
    lift $ UserRepo.save user
    pure user
  token <- lift $ TokenGateway.generate userId User.tokeExpiresInSec
  pure $ User.mkAuthorizedUser user token

----------------------------------------------------------------------------------------------------
-- Authentication

data AuthenticationCommand = AuthenticationCommand
  { authenticationCommandEmail :: Email,
    authenticationCommandPassword :: Password
  }
  deriving (Show, Eq, Generic)

mkAuthenticationCommand :: Text -> Text -> Maybe AuthenticationCommand
mkAuthenticationCommand email password = do
  AuthenticationCommand
    <$> mkEmail email
    <*> mkPassword password

data AuthenticationError
  = AuthenticationErrorUserNotFound
  | AuthenticationErrorInvalidPassword
  deriving (Show, Eq)

authentication ::
  (MonadIO m, UserRepository m, PasswordGateway m, TokenGateway m) =>
  AuthenticationCommand ->
  m (Either AuthenticationError AuthorizedUser)
authentication (AuthenticationCommand email password) = runExceptT $ do
  user <- UserRepo.findByEmail email !? AuthenticationErrorUserNotFound
  whenM (lift $ PasswordGateway.isValidPassword (userHashedPassword user) password)
    $ throwE AuthenticationErrorInvalidPassword
  token <- lift $ TokenGateway.generate (userId user) User.tokeExpiresInSec
  pure $ User.mkAuthorizedUser user token

----------------------------------------------------------------------------------------------------
-- Update User

data UpdateUserCommand = UpdateUserCommand
  { updateUserCommandToken :: Token,
    updateUserCommandUserName :: Maybe Username,
    updateUserCommandEmail :: Maybe Email,
    updateUserCommandPassword :: Maybe Password,
    updateUserCommandBio :: Maybe Bio,
    updateUserCommandImage :: Maybe (Maybe Image)
  }
  deriving (Show, Eq, Generic)

mkUpdateUserCommand ::
  Token ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe (Maybe Text) ->
  Maybe UpdateUserCommand
mkUpdateUserCommand token username email password bio image = do
  UpdateUserCommand token
    <$> traverse mkUsername username
    <*> traverse mkEmail email
    <*> traverse mkPassword password
    <*> pure (mkBio <$> bio)
    <*> pure (mkImage <$> image)

data UpdateUserError
  = UpdateUserErrorInvalidToken
  | UpdateUserErrorInvalidPassword
  | UpdateUserErrorUserNotFound
  | UpdateUserErrorUsernameAlreadyExists
  deriving (Show, Eq)

updateUser ::
  (MonadIO m, UserRepository m, PasswordGateway m, TokenGateway m, Tx m) =>
  UpdateUserCommand ->
  m (Either UpdateUserError AuthorizedUser)
updateUser (UpdateUserCommand token username email password bio image) = runExceptT $ do
  liftIO $ print token
  userId <- TokenGateway.verify token !? UpdateUserErrorInvalidToken
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
  pure $ User.mkAuthorizedUser user token

----------------------------------------------------------------------------------------------------
-- Follow User

data FollowUserCommand = FollowUserCommand
  { followUserCommandToken :: Token,
    followUserCommandUsername :: Username
  }
  deriving (Show, Eq, Generic)

mkFollowUserCommand :: Token -> Text -> Maybe FollowUserCommand
mkFollowUserCommand token username = do
  FollowUserCommand token <$> mkUsername username

data FollowUserError
  = FollowUserErrorInvalidToken
  | FollowUserErrorUserNotFound
  | FollowUserErrorCantFollowSelf
  | FollowUserErrorAlreadyFollowing
  deriving (Show, Eq)

followUser ::
  (MonadIO m, UserRepository m, TokenGateway m, Tx m) =>
  FollowUserCommand ->
  m (Either FollowUserError Profile)
followUser (FollowUserCommand token username) = runExceptT $ do
  followerId <- TokenGateway.verify token !? FollowUserErrorInvalidToken
  withTx $ do
    followee <- UserRepo.findByUsername username !? FollowUserErrorUserNotFound
    isAlreadyFollow <- lift $ UserRepo.hasFollowing followerId (userId followee)
    when isAlreadyFollow $ throwE FollowUserErrorAlreadyFollowing
    when (followerId == userId followee) $ throwE FollowUserErrorCantFollowSelf
    success <- lift $ UserRepo.follow followerId (userId followee)
    pure $ User.mkProfile followee success

----------------------------------------------------------------------------------------------------
-- Unfollow User

data UnfollowUserCommand = UnfollowUserCommand
  { unfollowUserCommandToken :: Token,
    unfollowUserCommandUsername :: Username
  }
  deriving (Show, Eq, Generic)

mkUnfollowUserCommand :: Token -> Text -> Maybe UnfollowUserCommand
mkUnfollowUserCommand token username = do
  UnfollowUserCommand token <$> mkUsername username

data UnfollowUserError
  = UnfollowUserErrorInvalidToken
  | UnfollowUserErrorUserNotFound
  deriving (Show, Eq)

unfollowUser :: (MonadIO m, UserRepository m, TokenGateway m, Tx m) => UnfollowUserCommand -> m (Either UnfollowUserError Profile)
unfollowUser (UnfollowUserCommand token username) = runExceptT $ do
  followerId <- TokenGateway.verify token !? UnfollowUserErrorInvalidToken
  withTx $ do
    followee <- UserRepo.findByUsername username !? UnfollowUserErrorUserNotFound
    success <- lift $ UserRepo.unfollow followerId (userId followee)
    pure $ User.mkProfile followee (not success)