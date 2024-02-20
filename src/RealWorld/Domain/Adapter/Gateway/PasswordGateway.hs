module RealWorld.Domain.Adapter.Gateway.PasswordGateway where

import RealWorld.Domain.Command.User.Value (HashedPassword, Password)
import Relude

class PasswordGateway m where
  hashPassword :: Password -> m (Maybe HashedPassword)
  isValidPassword :: HashedPassword -> Password -> m Bool
