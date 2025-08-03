{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Web.Auth where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON)
import qualified Data.List as List
import qualified Data.Text as T
import Network.Wai (Request (requestHeaders))
import qualified RealWorld.Infra.Gateway.JwtTokenGateway as JwtTokenGateway
import RealWorld.Infra.Web.ErrorResponse (unauthorized')
import Relude
import Servant (AuthProtect)
import Servant.Server.Experimental.Auth (
  AuthHandler,
  AuthServerData,
  mkAuthHandler,
 )

data ApiAuth = ApiAuth {userId :: Text, token :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

type instance AuthServerData (AuthProtect "apiAuth") = ApiAuth

type AuthContext = (AuthHandler Request ApiAuth ': '[])

authTokenHandler :: Text -> AuthHandler Request ApiAuth
authTokenHandler secret = mkAuthHandler handler
 where
  handler req = do
    token <- whenNothingM (pure $ parseAuthorizationHeader req) $ do
      throwError $ unauthorized' "Unauthorized"
    userId <- whenNothingM (liftIO $ JwtTokenGateway.getJwtUserId secret token) $ do
      throwError $ unauthorized' "Unauthorized"
    pure $ ApiAuth userId token

parseAuthorizationHeader :: Request -> Maybe Text
parseAuthorizationHeader req = do
  header <- List.lookup "Authorization" (requestHeaders req)
  case T.splitOn " " (decodeUtf8 header) of
    ["Token", token] -> pure token
    _ -> Nothing
