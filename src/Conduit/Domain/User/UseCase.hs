module Conduit.Domain.User.UseCase where

import Conduit.Domain.Repo (Tx (withTx))
import Conduit.Domain.User.Entity
  ( AuthenticationCommand (..),
    AuthorizedUser,
    RegistrationCommand (..),
    Token,
    User (userHashedPassword, userId),
  )
import qualified Conduit.Domain.User.Entity as User
import Conduit.Domain.User.Error
  ( UserError (..),
  )
import Conduit.Domain.User.Gateway.Token (TokenGateway)
import qualified Conduit.Domain.User.Gateway.Token as TokenGateway
import Conduit.Domain.User.Repo (UserRepository)
import qualified Conduit.Domain.User.Repo as UserRepo
import Conduit.Domain.User.Service.Password (PasswordService)
import qualified Conduit.Domain.User.Service.Password as PasswordService
import Conduit.Util.Maybe (justToNothing)
import Control.Error (throwE)
import Control.Error.Util ((!?))
import Data.Time (getCurrentTime)
import Data.ULID (getULID)
import Relude

registration ::
  (MonadIO m, UserRepository m, TokenGateway m, PasswordService m, Tx m) =>
  RegistrationCommand ->
  m (Either UserError AuthorizedUser)
registration (RegistrationCommand userName email password) = runExceptT $ do
  userId <- liftIO getULID
  createdAt <- liftIO getCurrentTime
  hashedPassword <- PasswordService.hashPassword password !? PasswordInvalid
  user <- withTx $ do
    (justToNothing <$> UserRepo.findByUsername userName) !? UserAlreadyExists
    (justToNothing <$> UserRepo.findByEmail email) !? UserAlreadyExists
    user <- hoistEither $ User.mkUser userId userName email hashedPassword createdAt
    lift $ UserRepo.create user
    pure user
  token <- lift $ TokenGateway.generate userId
  pure $ User.mkAuthorizedUser user token

authentication ::
  (MonadIO m, UserRepository m, PasswordService m, TokenGateway m) =>
  AuthenticationCommand ->
  m (Either UserError AuthorizedUser)
authentication (AuthenticationCommand email password) = runExceptT $ do
  user <- UserRepo.findByEmail email !? UserNotFound
  whenM (lift $ PasswordService.isValidPassword (userHashedPassword user) password)
    $ throwE PasswordInvalid
  token <- lift $ TokenGateway.generate (userId user)
  pure $ User.mkAuthorizedUser user token

getCurrentUser ::
  (MonadIO m, UserRepository m, TokenGateway m) =>
  Token ->
  m (Either UserError AuthorizedUser)
getCurrentUser token = runExceptT $ do
  userId <- TokenGateway.verify token !? TokenInvalid
  user <- UserRepo.findById userId !? UserNotFound
  pure $ User.mkAuthorizedUser user token
