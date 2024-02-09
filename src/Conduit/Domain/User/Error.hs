module Conduit.Domain.User.Error where

import Relude

data UserError
  = UserNameInvalid
  | EmailInvalid
  | PasswordInvalid
  | UserNotFound
  | UserAlreadyExists
  deriving (Show, Eq)