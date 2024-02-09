{-# LANGUAGE FlexibleContexts #-}

module Conduit.Infra.Gateway.JwtToken where

import Conduit.Domain.User.Entity (Token (..))
import qualified Data.Aeson.Types as JSON
import Data.Has
import Data.ULID (ULID)
import Relude hiding (State)
import Web.JWT
  ( ClaimsMap (ClaimsMap),
    JWTClaimsSet (unregisteredClaims),
    encodeSigned,
    hmacSecret,
  )

signJWT :: Map Text JSON.Value -> Text -> Text
signJWT claims secret =
  encodeSigned (hmacSecret secret) mempty claimSet
  where
    claimSet = mempty {unregisteredClaims = ClaimsMap claims}

generate :: (Has Text r, MonadReader r m) => ULID -> m Token
generate userId = do
  secret <- asks getter
  pure $ Token $ signJWT (fromList [("userId", JSON.String $ show userId)]) secret

verify :: Token -> m (Maybe ULID)
verify = undefined