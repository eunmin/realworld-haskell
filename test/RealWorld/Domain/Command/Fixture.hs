module RealWorld.Domain.Command.Fixture where

import Data.Time (getCurrentTime)
import Data.ULID (ULID, getULID)
import RealWorld.Domain.Command.User.Entity.User (User, mkUser)
import RealWorld.Domain.Command.User.Value
  ( Email (Email),
    HashedPassword (HashedPassword),
    Password,
    Token,
    Username (Username),
  )
import Relude

data Fixture m = Fixture
  { -- UserReposiotry
    _saveUser :: User -> m Bool,
    _findUserById :: ULID -> m (Maybe User),
    _findUserByUsername :: Username -> m (Maybe User),
    _findUserByEmail :: Email -> m (Maybe User),
    _followUser :: ULID -> ULID -> m Bool,
    _unfollowUser :: ULID -> ULID -> m Bool,
    _hasFollowingUser :: ULID -> ULID -> m Bool,
    -- TokenGateway
    _generateToken :: ULID -> Int -> m Token,
    _verifyToken :: Token -> m (Maybe ULID),
    -- PasswordGateway
    _hashPassword :: Password -> m (Maybe HashedPassword),
    _isValidPassword :: HashedPassword -> Password -> m Bool
  }

emptyFixture :: Fixture m
emptyFixture =
  Fixture
    { -- UserReposiotry
      _saveUser = const $ error "_saveUser is not implemented",
      _findUserById = const $ error "_findUserById is not implemented",
      _findUserByUsername = const $ error "_findUserByUsername is not implemented",
      _findUserByEmail = const $ error "_findUserByEmail is not implemented",
      _followUser = const $ error "_followUser is not implemented",
      _unfollowUser = const $ error "_unfollowUser is not implemented",
      _hasFollowingUser = const $ error "_hasFollowingUser is not implemented",
      -- TokenGateway
      _generateToken = const $ error "_generateToken is not implemented",
      _verifyToken = const $ error "_verifyToken is not implemented",
      -- PasswordGateway
      _hashPassword = const $ error "_hashPassword is not implemented",
      _isValidPassword = const $ error "_isValidPassword is not implemented"
    }

dispatch0 ::
  (MonadIO m, MonadReader r m) =>
  (r -> IO b) ->
  m b
dispatch0 getter = do
  func <- asks getter
  liftIO func

dispatch1 ::
  (MonadIO m, MonadReader r m) =>
  (r -> a -> IO b) ->
  (a -> m b)
dispatch1 getter param = do
  func <- asks getter
  liftIO $ func param

dispatch2 ::
  (MonadIO m, MonadReader r m) =>
  (r -> a -> b -> IO c) ->
  (a -> b -> m c)
dispatch2 getter param1 param2 = do
  func <- asks getter
  liftIO $ func param1 param2

dispatch3 ::
  (MonadIO m, MonadReader r m) =>
  (r -> a -> b -> c -> IO d) ->
  (a -> b -> c -> m d)
dispatch3 getter param1 param2 param3 = do
  func <- asks getter
  liftIO $ func param1 param2 param3

dispatch4 ::
  (MonadIO m, MonadReader r m) =>
  (r -> a -> b -> c -> d -> IO e) ->
  (a -> b -> c -> d -> m e)
dispatch4 getter param1 param2 param3 param4 = do
  func <- asks getter
  liftIO $ func param1 param2 param3 param4

generateUser :: IO User
generateUser = do
  now <- getCurrentTime
  userId <- getULID
  let username = Username "username"
  let email = Email "username@example.com"
  let hashedPassword = HashedPassword "hashed"
  pure $ mkUser userId username email hashedPassword now
