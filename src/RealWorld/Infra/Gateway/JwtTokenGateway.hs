{-# LANGUAGE FlexibleContexts #-}

module RealWorld.Infra.Gateway.JwtTokenGateway where

import Data.Aeson.Types qualified as JSON
import Data.Has (Has (getter))
import Data.Map.Strict (lookup)
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.ULID (ULID)
import RealWorld.Domain.Command.User.Value (Token (..))
import Relude hiding (State, exp)
import Web.JWT (
  ClaimsMap (..),
  JWT,
  JWTClaimsSet (exp, unregisteredClaims),
  VerifiedJWT,
  decodeAndVerifySignature,
  encodeSigned,
  hmacSecret,
  numericDate,
  toVerify,
 )
import Web.JWT qualified as JWT
import Prelude hiding (State, exp)

signJWT :: Map Text JSON.Value -> Text -> NominalDiffTime -> NominalDiffTime -> Text
signJWT claims secret currentTime expiresIn =
  encodeSigned (hmacSecret secret) mempty claimSet
  where
    claimSet =
      mempty
        { unregisteredClaims = ClaimsMap claims
        , exp = numericDate $ currentTime + expiresIn
        }

unsignJWT :: Text -> Text -> NominalDiffTime -> Maybe (Map Text JSON.Value)
unsignJWT jwt secret currentTime = do
  verifiedJwt <- decodeAndVerifySignature (toVerify . hmacSecret $ secret) jwt
  ifM
    (isExpired verifiedJwt)
    Nothing
    $ pure (unClaimsMap . unregisteredClaims . JWT.claims $ verifiedJwt)
  where
    isExpired :: JWT VerifiedJWT -> Maybe Bool
    isExpired verifiedJWT =
      (<) <$> (exp . JWT.claims $ verifiedJWT) <*> numericDate currentTime

generate :: (Has Text r, MonadReader r m, MonadIO m) => ULID -> Int -> m Token
generate userId expiresInSec = do
  secret <- asks getter
  now <- liftIO getPOSIXTime
  pure $
    Token $
      signJWT
        (fromList [("userId", JSON.String $ show userId)])
        secret
        now
        (fromInteger . toInteger $ expiresInSec)

verify :: (Has Text r, MonadReader r m, MonadIO m) => Token -> m (Maybe ULID)
verify (Token token) = do
  secret :: Text <- asks getter
  now <- liftIO getPOSIXTime
  pure $ do
    claims <- unsignJWT token secret now
    userId <- lookup "userId" claims
    valutToText userId >>= readMaybe . toString
  where
    valutToText :: JSON.Value -> Maybe Text
    valutToText (JSON.String t) = Just t
    valutToText _ = Nothing
