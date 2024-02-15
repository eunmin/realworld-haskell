module RealWorld.Domain.User.Gateway.Token where

import Data.ULID (ULID)
import RealWorld.Domain.User.Entity (Token)
import Relude

class TokenGateway m where
  generate :: ULID -> m Token
  verify :: Token -> m (Maybe ULID)