module RealWorld.Domain.User.Gateway.Password where

import RealWorld.Domain.User.Types (HashedPassword, Password)
import Relude

class PasswordGateway m where
  hashPassword :: Password -> m (Maybe HashedPassword)
  isValidPassword :: HashedPassword -> Password -> m Bool