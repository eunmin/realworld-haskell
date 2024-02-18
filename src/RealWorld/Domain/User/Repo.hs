module RealWorld.Domain.User.Repo where

import Data.ULID (ULID)
import RealWorld.Domain.User.Types
import Relude

class UserRepository m where
  save :: User -> m ()
  findById :: ULID -> m (Maybe User)
  findByUsername :: Username -> m (Maybe User)
  findByEmail :: Email -> m (Maybe User)
  follow :: ULID -> ULID -> m Bool
  unfollow :: ULID -> ULID -> m Bool
  hasFollowing :: ULID -> ULID -> m Bool
