{-# LANGUAGE DeriveAnyClass #-}

module Conduit.Infra.Web.Controller.User where

import Conduit.Domain.Repo (Tx)
import Conduit.Domain.User.Entity (mkRegisterCommand)
import Conduit.Domain.User.Gateway.Token (TokenGateway)
import Conduit.Domain.User.Repo (UserRepository)
import Conduit.Domain.User.Service.Password (PasswordService)
import qualified Conduit.Domain.User.UseCase as UserUseCase
import Conduit.Infra.Json ()
import Conduit.Infra.Web.Error (errorResponse)
import Data.Aeson (FromJSON)
import Network.HTTP.Types.Status (status400)
import Relude
import Web.Scotty.Trans (ActionT, json, jsonData)

data RegisterInput = RegisterInput
  { email :: Text,
    userName :: Text,
    password :: Text
  }
  deriving (Show, Generic, FromJSON)

register :: (MonadIO m, UserRepository m, TokenGateway m, PasswordService m, Tx m) => ActionT LText m ()
register = do
  RegisterInput {..} <- jsonData
  case mkRegisterCommand userName email password of
    Left err -> errorResponse status400 $ show err
    Right command -> do
      result <- lift $ UserUseCase.register command
      case result of
        Right authorizedUser -> json authorizedUser
        Left err -> errorResponse status400 $ show err