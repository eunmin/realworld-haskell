module RealWorld.Domain.Adapter.Repository.UserRepository where

import Data.ULID (ULID)
import RealWorld.Domain.Command.User.Entity.User (User)
import RealWorld.Domain.Command.User.Value (Email, Username)
import Relude

class UserRepository m where
  save :: User -> m Bool
  findById :: ULID -> m (Maybe User)
  findByUsername :: Username -> m (Maybe User)
  findByEmail :: Email -> m (Maybe User)
  follow :: ULID -> ULID -> m Bool
  unfollow :: ULID -> ULID -> m Bool
  hasFollowing :: ULID -> ULID -> m Bool
