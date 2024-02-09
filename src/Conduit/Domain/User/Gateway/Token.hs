module Conduit.Domain.User.Gateway.Token where

import Conduit.Domain.User.Entity (Token)
import Data.ULID (ULID)
import Relude

class TokenGateway m where
  generate :: ULID -> m Token
  verify :: Token -> m (Maybe ULID)