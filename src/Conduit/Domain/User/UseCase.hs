module Conduit.Domain.User.UseCase where

import Conduit.Domain.Repo (Tx (withTx))
import Conduit.Domain.User.Entity
import Conduit.Domain.User.Error
import Conduit.Domain.User.Gateway.Token (TokenGateway)
import qualified Conduit.Domain.User.Gateway.Token as TokenGateway
import Conduit.Domain.User.Repo (UserRepository)
import qualified Conduit.Domain.User.Repo as UserRepo
import Conduit.Domain.User.Service.Password (PasswordService)
import qualified Conduit.Domain.User.Service.Password as PasswordService
import Conduit.Util.Maybe (justToNothing)
import Control.Error.Util ((!?))
import Data.Time (getCurrentTime)
import Data.ULID (getULID)
import Relude

register ::
  (MonadIO m, UserRepository m, TokenGateway m, PasswordService m, Tx m) =>
  RegisterCommand ->
  m (Either UserError AuthorizedUser)
register (RegisterCommand userName email password) = runExceptT $ do
  userId <- liftIO getULID
  createdAt <- liftIO getCurrentTime
  user <- withTx $ do
    (justToNothing <$> UserRepo.findByUsername userName) !? UserAlreadyExists
    (justToNothing <$> UserRepo.findByEmail email) !? UserAlreadyExists
    hashedPassword <- PasswordService.hashPassword password !? PasswordInvalid
    user <- hoistEither $ mkUser userId userName email hashedPassword createdAt
    lift $ UserRepo.create user
    pure user
  token <- lift $ TokenGateway.generate userId
  pure $ mkAuthorizedUser user token
