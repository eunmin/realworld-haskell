{-# LANGUAGE FlexibleContexts #-}

module RealWorld.Infra.Gateway.JwtTokenGateway where

import Control.Lens ((&), (?~))
import Crypto.JOSE.Compact (decodeCompact, encodeCompact)
import qualified Crypto.JOSE.JWK as JWK
import Crypto.JWT
  ( Audience (Audience),
    ClaimsSet,
    HasClaimsSet (claimAud, claimExp, claimIat, claimIss, claimsSet),
    JWK,
    JWTError,
    NumericDate (NumericDate),
    SignedJWT,
    bestJWSAlg,
    defaultJWTValidationSettings,
    emptyClaimsSet,
    fromOctets,
    newJWSHeader,
    runJOSE,
    signJWT,
    verifyJWT,
  )
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Object),
    withObject,
    (.:),
  )
import qualified Data.Aeson.KeyMap as M
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as L
import Data.Has (Has (getter))
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.ULID (ULID)
import RealWorld.Domain.Command.User.Value (Token (..))
import Relude hiding (State, exp, (&))

data AuthJwt = AuthJwt
  { claimSet :: ClaimsSet,
    userId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance HasClaimsSet AuthJwt where
  claimsSet f s = fmap (\a' -> s {claimSet = a'}) (f (claimSet s))

instance FromJSON AuthJwt where
  parseJSON = withObject "AuthJwt" $ \o ->
    AuthJwt
      <$> parseJSON (Object o)
      <*> o
      .: "userId"

instance ToJSON AuthJwt where
  toJSON s =
    ins "userId" (userId s) (toJSON (claimSet s))
    where
      ins k v (JSON.Object o) = Object $ M.insert k (toJSON v) o
      ins _ _ a = a

mkClaimSet :: NominalDiffTime -> IO ClaimsSet
mkClaimSet tokenExpiry = do
  t <- getCurrentTime
  pure
    $ emptyClaimsSet
    & claimIss ?~ "realworld"
    & claimAud ?~ Audience ["realworld"]
    & claimIat ?~ NumericDate t
    & claimExp ?~ NumericDate (addUTCTime tokenExpiry t)

doJwtSign :: JWK -> AuthJwt -> IO (Either JWTError SignedJWT)
doJwtSign jwk super = runJOSE $ do
  alg <- bestJWSAlg jwk
  signJWT jwk (newJWSHeader ((), alg)) super

generate :: (Has Text r, MonadReader r m, MonadIO m) => ULID -> Int -> m Token
generate userId expiresInSec = do
  secret :: Text <- asks getter
  claimSet <- liftIO $ mkClaimSet (secondsToNominalDiffTime (fromIntegral expiresInSec))
  let jwk = JWK.fromOctets (encodeUtf8 secret :: L.ByteString)
      super = AuthJwt claimSet (show userId)
  result <- liftIO $ doJwtSign jwk super
  case result of
    Left _ -> error "Failed to sign JWT"
    Right jwt -> pure $ Token $ decodeUtf8 $ encodeCompact jwt

verify :: (Has Text r, MonadReader r m, MonadIO m) => Token -> m (Maybe ULID)
verify (Token token) = do
  secret :: Text <- asks getter
  result :: (Either JWTError AuthJwt) <- liftIO $ runJOSE $ do
    let jwk = fromOctets (encodeUtf8 secret :: L.ByteString)
        audCheck = (== "realworld")
    jwt :: SignedJWT <- decodeCompact $ encodeUtf8 token
    verifyJWT (defaultJWTValidationSettings audCheck) jwk jwt
  case result of
    Left _ -> error "Failed to verify JWT"
    Right super' -> pure $ readMaybe $ toString super'.userId
