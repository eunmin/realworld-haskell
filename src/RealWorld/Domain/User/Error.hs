module RealWorld.Domain.User.Error where

import Relude

data UserError
  = UserNameInvalid
  | EmailInvalid
  | PasswordInvalid
  | UserNotFound
  | UserAlreadyExists
  | TokenInvalid
  deriving (Show, Eq)