{-# LANGUAGE DeriveAnyClass #-}

module Conduit.Infra.Web.Controller.User where

import Conduit.Domain.User.Entity (AuthorizedUser (..), Email (..), Token (..), UserName (..))
import Conduit.Domain.User.Gateway.Token (TokenGateway)
import Conduit.Domain.User.Repo (UserRepository)
import Conduit.Domain.User.Service.Password (PasswordService)
import qualified Conduit.Domain.User.UseCase as UserUseCase
import Conduit.Infra.Json ()
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (getCurrentTime)
import Data.ULID (getULID)
import Network.HTTP.Types.Status (status400)
import Relude
import Web.Scotty.Trans

data RegisterInput = RegisterInput
  { email :: Text,
    userName :: Text,
    password :: Text
  }
  deriving (Show, Generic, FromJSON)

data Error = Error
  {message :: Text}
  deriving (Show, Generic, ToJSON)

register :: (MonadIO m, UserRepository m, TokenGateway m, PasswordService m) => ActionT LText m ()
register = do
  RegisterInput {..} <- jsonData
  result <- lift $ UserUseCase.register userName email password
  case result of
    Right authorizedUser -> json authorizedUser
    Left err -> do
      status status400
      json $ Error $ show err

-- json
-- \$ AuthorizedUser
--   { authorizedUserId = userId,
--     authorizedUserUsername = userName,
--     authorizedUserEmail = email,
--     authorizedUserToken = token,
--     authorizedUserCreatedAt = createdAt,
--     authorizedUserUpdatedAt = Nothing
--   }
-- where
--   userName = UserName "test"
--   email = Email "test@test.com"
--   token = Token "test"