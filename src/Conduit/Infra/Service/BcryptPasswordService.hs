module Conduit.Infra.Service.BcryptPasswordService where

import Conduit.Domain.User.Entity (HashedPassword (..), Password (..))
import Crypto.BCrypt
  ( hashPasswordUsingPolicy,
    slowerBcryptHashingPolicy,
  )
import Relude

hashPassword :: (MonadIO m) => Password -> m (Maybe HashedPassword)
hashPassword (Password password) = do
  hash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy $ encodeUtf8 password
  pure $ HashedPassword <$> (decodeUtf8 <$> hash)
