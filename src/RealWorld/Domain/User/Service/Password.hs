module RealWorld.Domain.User.Service.Password where

import RealWorld.Domain.User.Entity (HashedPassword, Password)
import Relude

class PasswordService m where
  hashPassword :: Password -> m (Maybe HashedPassword)
  isValidPassword :: HashedPassword -> Password -> m Bool