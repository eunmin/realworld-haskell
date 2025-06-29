{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module RealWorld.Domain.Command.User.Entity.User where

import Data.Time (UTCTime)
import Data.ULID (ULID)
import RealWorld.Domain.Command.User.Value
  ( Bio,
    Email,
    HashedPassword,
    Image,
    Username,
    emptyBio,
  )
import Relude

----------------------------------------------------------------------------------------------------
-- User

data User = User
  { userId :: ULID,
    username :: Username,
    email :: Email,
    hashedPassword :: HashedPassword,
    bio :: Bio,
    image :: Maybe Image,
    createdAt :: UTCTime,
    updatedAt :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)

mkUser :: ULID -> Username -> Email -> HashedPassword -> UTCTime -> User
mkUser userId username email hashedPassword createdAt =
  User
    { userId = userId,
      username = username,
      email = email,
      hashedPassword = hashedPassword,
      bio = emptyBio,
      image = Nothing,
      createdAt = createdAt,
      updatedAt = Nothing
    }

update ::
  User ->
  Maybe Username ->
  Maybe Email ->
  Maybe HashedPassword ->
  Maybe Bio ->
  Maybe (Maybe Image) ->
  User
update user username email hashedPassword bio image =
  user
    { username = username ?: user.username,
      email = email ?: user.email,
      hashedPassword = hashedPassword ?: user.hashedPassword,
      bio = bio ?: user.bio,
      image = image ?: user.image
    }
