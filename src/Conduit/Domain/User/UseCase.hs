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
  Text ->
  Text ->
  Text ->
  m (Either UserError AuthorizedUser)
register userName email password = withTx $ runExceptT $ do
  -- error "Not implemented"
  userName' <- hoistEither $ mkUserName userName
  email' <- hoistEither $ mkEmail email
  (justToNothing <$> UserRepo.findByUsername userName') !? UserAlreadyExists
  (justToNothing <$> UserRepo.findByEmail email') !? UserAlreadyExists
  password' <- hoistEither $ mkPassword password
  hashedPassword <- PasswordService.hashPassword password' !? PasswordInvalid
  userId <- liftIO getULID
  createdAt <- liftIO getCurrentTime
  user <- hoistEither $ mkUser userId userName' email' hashedPassword createdAt
  lift $ UserRepo.create user
  token <- lift $ TokenGateway.generate userId
  pure $ mkAuthorizedUser user token
