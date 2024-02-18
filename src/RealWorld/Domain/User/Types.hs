{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module RealWorld.Domain.User.Types where

import Data.Time (UTCTime)
import Data.ULID (ULID)
import RealWorld.Util.BoundedText (BoundedText, mkBoundedText)
import Relude
import Text.Regex.PCRE.Heavy (Regex, re, (=~))

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
    { userUsername = fromMaybe (userUsername user) username,
      userEmail = fromMaybe (userEmail user) email,
      userHashedPassword = fromMaybe (userHashedPassword user) hashedPassword,
      userBio = fromMaybe (userBio user) bio,
      userImage = fromMaybe (userImage user) image
    }

----------------------------------------------------------------------------------------------------
-- Token

newtype Token = Token {unToken :: Text}
  deriving (Show, Eq, Generic)

tokeExpiresInSec :: Int
tokeExpiresInSec = 60 * 60 * 24 -- 1 day

----------------------------------------------------------------------------------------------------
-- AuthorizedUser

data AuthorizedUser = AuthorizedUser
  { authorizedUserUsername :: Username,
    authorizedUserEmail :: Email,
    authorizedUserToken :: Token,
    authorizedUserBio :: Bio,
    authorizedUserImage :: Maybe Image
  }
  deriving (Show, Eq, Generic)

mkAuthorizedUser :: User -> Token -> AuthorizedUser
mkAuthorizedUser User {userUsername, userEmail, userBio, userImage} token =
  AuthorizedUser
    { authorizedUserUsername = userUsername,
      authorizedUserEmail = userEmail,
      authorizedUserToken = token,
      authorizedUserBio = userBio,
      authorizedUserImage = userImage
    }

----------------------------------------------------------------------------------------------------
-- Username

newtype Username = Username {unUserName :: BoundedText 3 128}
  deriving (Show, Eq, Generic)

mkUsername :: Text -> Maybe Username
mkUsername username =
  Username <$> mkBoundedText username

----------------------------------------------------------------------------------------------------
-- Email

newtype Email = Email {unEmail :: Text}
  deriving (Show, Eq, Generic)

emailRegex :: Regex
emailRegex = [re|^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$|]

mkEmail :: Text -> Maybe Email
mkEmail email =
  if email =~ emailRegex
    then Just $ Email email
    else Nothing

----------------------------------------------------------------------------------------------------
-- Password

newtype Password = Password {unPassword :: Text}
  deriving (Show, Eq, Generic)

passwordRegex :: Regex
passwordRegex = [re|^(?=.*[A-Za-z])(?=.*\d)(?=.*[@$!%*#?&])[A-Za-z\d@$!%*#?&]{12,}$|]

mkPassword :: Text -> Maybe Password
mkPassword password =
  if password =~ passwordRegex
    then Just $ Password password
    else Nothing

newtype HashedPassword = HashedPassword {unHashedPassword :: Text}
  deriving (Show, Eq, Generic)

----------------------------------------------------------------------------------------------------
-- Bio
newtype Bio = Bio {unBio :: Text}
  deriving (Show, Eq, Generic)

mkBio :: Text -> Bio
mkBio = Bio

emptyBio :: Bio
emptyBio = Bio ""

----------------------------------------------------------------------------------------------------
-- Image

newtype Image = Image {unImage :: Text}
  deriving (Show, Eq, Generic)

mkImage :: Maybe Text -> Maybe Image
mkImage image = Image <$> image

----------------------------------------------------------------------------------------------------
-- Profile

data Profile = Profile
  { profileUsername :: Username,
    profileBio :: Bio,
    profileImage :: Maybe Image,
    profileFollowing :: Bool
  }
  deriving (Show, Eq, Generic)

mkProfile :: User -> Bool -> Profile
mkProfile User {..} following =
  Profile
    { profileUsername = userUsername,
      profileBio = userBio,
      profileImage = userImage,
      profileFollowing = following
    }