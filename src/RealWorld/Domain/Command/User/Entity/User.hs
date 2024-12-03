{-# LANGUAGE StrictData #-}

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

----------------------------------------------------------------------------------------------------
-- User

data User = User
  { userId :: ULID,
    userUsername :: Username,
    userEmail :: Email,
    userHashedPassword :: HashedPassword,
    userBio :: Bio,
    userImage :: Maybe Image,
    userCreatedAt :: UTCTime,
    userUpdatedAt :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic)

mkUser :: ULID -> Username -> Email -> HashedPassword -> UTCTime -> User
mkUser userId username email hashedPassword createdAt =
  User
    { userId = userId,
      userUsername = username,
      userEmail = email,
      userHashedPassword = hashedPassword,
      userBio = emptyBio,
      userImage = Nothing,
      userCreatedAt = createdAt,
      userUpdatedAt = Nothing
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
    { userUsername = username ?: userUsername user,
      userEmail = email ?: userEmail user,
      userHashedPassword = hashedPassword ?: userHashedPassword user,
      userBio = bio ?: userBio user,
      userImage = image ?: userImage user
    }
