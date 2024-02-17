{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module RealWorld.Domain.User.Entity where

import Data.Time (UTCTime)
import Data.ULID (ULID)
import RealWorld.Domain.User.Error (UserError (..))
import RealWorld.Util.BoundedText (BoundedText, mkBoundedText)
import Relude
import Text.Regex.PCRE.Heavy (Regex, re, (=~))

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

mkUser :: ULID -> Username -> Email -> HashedPassword -> UTCTime -> Either UserError User
mkUser userId username email hashedPassword createdAt =
  Right
    $ User
      { userId = userId,
        userUsername = username,
        userEmail = email,
        userHashedPassword = hashedPassword,
        userBio = emptyBio,
        userImage = Nothing,
        userCreatedAt = createdAt,
        userUpdatedAt = Nothing
      }

-- Token

newtype Token = Token {unToken :: Text}
  deriving (Show, Eq, Generic)

tokeExpiresInSec :: Int
tokeExpiresInSec = 60 * 60 * 24 -- 1 day

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

-- Username

newtype Username = Username {unUserName :: BoundedText 3 128}
  deriving (Show, Eq, Generic)

mkUsername :: Text -> Either UserError Username
mkUsername username =
  Username <$> maybeToRight UserNameInvalid (mkBoundedText username)

-- Email

newtype Email = Email {unEmail :: Text}
  deriving (Show, Eq, Generic)

emailRegex :: Regex
emailRegex = [re|^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$|]

mkEmail :: Text -> Either UserError Email
mkEmail email =
  if email =~ emailRegex
    then Right $ Email email
    else Left EmailInvalid

-- Password

newtype Password = Password {unPassword :: Text}
  deriving (Show, Eq, Generic)

passwordRegex :: Regex
passwordRegex = [re|^(?=.*[A-Za-z])(?=.*\d)(?=.*[@$!%*#?&])[A-Za-z\d@$!%*#?&]{12,}$|]

mkPassword :: Text -> Either UserError Password
mkPassword password =
  if password =~ passwordRegex
    then Right $ Password password
    else Left PasswordInvalid

newtype HashedPassword = HashedPassword {unHashedPassword :: Text}
  deriving (Show, Eq, Generic)

-- Bio
newtype Bio = Bio {unBio :: Text}
  deriving (Show, Eq, Generic)

mkBio :: Text -> Either UserError Bio
mkBio bio = Right $ Bio bio

emptyBio :: Bio
emptyBio = Bio ""

-- Image

newtype Image = Image {unImage :: Text}
  deriving (Show, Eq, Generic)

mkImage :: Maybe Text -> Either UserError (Maybe Image)
mkImage image = Right $ Image <$> image

-- RegistrationCommand
data RegistrationCommand = RegistrationCommand
  { registrationCommandUserName :: Username,
    registrationCommandEmail :: Email,
    registrationCommandPassword :: Password
  }
  deriving (Show, Eq, Generic)

mkRegistrationCommand :: Text -> Text -> Text -> Either UserError RegistrationCommand
mkRegistrationCommand username email password = do
  RegistrationCommand
    <$> mkUsername username
    <*> mkEmail email
    <*> mkPassword password

-- AuthenticationCommand

data AuthenticationCommand = AuthenticationCommand
  { authenticationCommandEmail :: Email,
    authenticationCommandPassword :: Password
  }
  deriving (Show, Eq, Generic)

mkAuthenticationCommand :: Text -> Text -> Either UserError AuthenticationCommand
mkAuthenticationCommand email password = do
  AuthenticationCommand
    <$> mkEmail email
    <*> mkPassword password

-- UpdateUserCommand

data UpdateUserCommand = UpdateUserCommand
  { updateUserCommand :: Token,
    updateUserCommandUserName :: Maybe Username,
    updateUserCommandEmail :: Maybe Email,
    updateUserCommandPassword :: Maybe Password,
    updateUserCommandBio :: Maybe Bio,
    updateUserCommandImage :: Maybe (Maybe Image)
  }
  deriving (Show, Eq, Generic)

mkUpdateUserCommand ::
  Token ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe (Maybe Text) ->
  Either UserError UpdateUserCommand
mkUpdateUserCommand token username email password bio image = do
  UpdateUserCommand token
    <$> traverse mkUsername username
    <*> traverse mkEmail email
    <*> traverse mkPassword password
    <*> traverse mkBio bio
    <*> traverse mkImage image

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