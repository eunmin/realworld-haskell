module Conduit.Domain.User.Repo where

import Conduit.Domain.User.Entity
import Data.ULID (ULID)
import Relude

class UserRepository m where
  create :: User -> m ()
  findById :: ULID -> m (Maybe User)
  findByUsername :: UserName -> m (Maybe User)
  findByEmail :: Email -> m (Maybe User)