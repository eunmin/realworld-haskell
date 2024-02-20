module RealWorld.Domain.Adapter.Gateway.TokenGateway where

import Data.ULID (ULID)
import RealWorld.Domain.Command.User.Value (Token)
import Relude

class TokenGateway m where
  generate :: ULID -> Int -> m Token
  verify :: Token -> m (Maybe ULID)