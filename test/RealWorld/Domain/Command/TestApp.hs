module RealWorld.Domain.Command.TestApp where

import Katip
  ( Katip,
    KatipContext,
    KatipContextT,
    initLogEnv,
    runKatipContextT,
  )
import RealWorld.Domain.Adapter.Gateway.PasswordGateway
  ( PasswordGateway (..),
  )
import RealWorld.Domain.Adapter.Gateway.TokenGateway
  ( TokenGateway (..),
  )
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager (..))
import RealWorld.Domain.Adapter.Repository.UserRepository
  ( UserRepository (..),
  )
import RealWorld.Domain.Command.Fixture
  ( Fixture
      ( _findUserByEmail,
        _findUserById,
        _findUserByUsername,
        _followUser,
        _generateToken,
        _hasFollowingUser,
        _hashPassword,
        _isValidPassword,
        _saveUser,
        _unfollowUser,
        _verifyToken
      ),
    dispatch1,
    dispatch2,
  )
import Relude

newtype TestApp a = TestApp
  { unTestApp :: ReaderT (Fixture IO) (KatipContextT IO) a
  }
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadReader (Fixture IO),
      MonadIO,
      KatipContext,
      Katip
    )

runApp :: Fixture IO -> TestApp a -> IO a
runApp fixture action = do
  le <- initLogEnv "RealWorld" "test"
  runKatipContextT le () mempty . usingReaderT fixture . unTestApp $ action

instance UserRepository TestApp where
  save = dispatch1 _saveUser
  findById = dispatch1 _findUserById
  findByUsername = dispatch1 _findUserByUsername
  findByEmail = dispatch1 _findUserByEmail
  follow = dispatch2 _followUser
  unfollow = dispatch2 _unfollowUser
  hasFollowing = dispatch2 _hasFollowingUser

instance TokenGateway TestApp where
  generate = dispatch2 _generateToken
  verify = dispatch1 _verifyToken

instance PasswordGateway TestApp where
  hashPassword = dispatch1 _hashPassword
  isValidPassword = dispatch2 _isValidPassword

instance TxManager TestApp where
  withTx = id
