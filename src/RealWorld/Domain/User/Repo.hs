module RealWorld.Domain.User.Repo where

import Data.ULID (ULID)
import RealWorld.Domain.User.Entity
import Relude

class UserRepository m where
  save :: User -> m ()
  findById :: ULID -> m (Maybe User)
  findByUsername :: Username -> m (Maybe User)
  findByEmail :: Email -> m (Maybe User)
