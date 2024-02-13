module Conduit.Infra.Service.BcryptPasswordService where

import Conduit.Domain.User.Entity (HashedPassword (..), Password (..))
import Crypto.BCrypt
  ( hashPasswordUsingPolicy,
    slowerBcryptHashingPolicy,
    validatePassword,
  )
import Relude

hashPassword :: (MonadIO m) => Password -> m (Maybe HashedPassword)
hashPassword (Password password) = do
  hash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy $ encodeUtf8 password
  pure $ HashedPassword <$> (decodeUtf8 <$> hash)

isValidPassword :: (Applicative m) => HashedPassword -> Password -> m Bool
isValidPassword (HashedPassword password) (Password hashedPassword) =
  pure $ validatePassword (encodeUtf8 hashedPassword) (encodeUtf8 password)