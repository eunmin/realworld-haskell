module RealWorld.Domain.User.Gateway.Token where

import Data.ULID (ULID)
import RealWorld.Domain.User.Types (Token)
import Relude

class TokenGateway m where
  generate :: ULID -> Int -> m Token
  verify :: Token -> m (Maybe ULID)