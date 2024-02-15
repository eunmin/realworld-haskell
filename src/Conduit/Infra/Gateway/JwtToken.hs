{-# LANGUAGE FlexibleContexts #-}

module Conduit.Infra.Gateway.JwtToken where

import Conduit.Domain.User.Entity (Token (..))
import qualified Data.Aeson.Types as JSON
import Data.Has
import Data.Map.Strict (lookup)
import Data.Text (unpack)
import Data.ULID (ULID)
import Relude hiding (State)
import Web.JWT
  ( ClaimsMap (..),
    JWTClaimsSet (unregisteredClaims),
    decodeAndVerifySignature,
    encodeSigned,
    hmacSecret,
    toVerify,
  )
import qualified Web.JWT as JWT

signJWT :: Map Text JSON.Value -> Text -> Text
signJWT claims secret =
  encodeSigned (hmacSecret secret) mempty claimSet
  where
    claimSet = mempty {unregisteredClaims = ClaimsMap claims}

unsignJWT :: Text -> Text -> Maybe (Map Text JSON.Value)
unsignJWT token secret = do
  jwt <- decodeAndVerifySignature (toVerify . hmacSecret $ secret) token
  pure $ unClaimsMap . unregisteredClaims . JWT.claims $ jwt

generate :: (Has Text r, MonadState r m) => ULID -> m Token
generate userId = do
  secret <- gets getter
  pure $ Token $ signJWT (fromList [("userId", JSON.String $ show userId)]) secret

verify :: (Has Text r, MonadState r m) => Token -> m (Maybe ULID)
verify (Token token) = do
  secret :: Text <- gets getter
  pure $ do
    claims <- unsignJWT token secret
    userId <- lookup "userId" claims
    valutToText userId >>= readMaybe . unpack
  where
    valutToText :: JSON.Value -> Maybe Text
    valutToText (JSON.String t) = Just t
    valutToText _ = Nothing