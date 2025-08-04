{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Gateway.BcryptPasswordGateway where

import Crypto.BCrypt (
  hashPasswordUsingPolicy,
  slowerBcryptHashingPolicy,
  validatePassword,
 )
import Effectful (Eff, IOE, (:>))
import RealWorld.Domain.Command.User.Value (HashedPassword (..), Password (..))
import Relude

hashPassword :: (IOE :> es) => Password -> Eff es (Maybe HashedPassword)
hashPassword (Password password) = do
  hash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy $ encodeUtf8 password
  pure $ HashedPassword <$> (decodeUtf8 <$> hash)

isValidPassword :: HashedPassword -> Password -> Eff es Bool
isValidPassword (HashedPassword password) (Password hashedPassword) =
  pure $ validatePassword (encodeUtf8 hashedPassword) (encodeUtf8 password)
