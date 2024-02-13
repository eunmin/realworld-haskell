{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module Conduit.Domain.User.Entity where

import Conduit.Domain.User.Error (UserError (..))
import Conduit.Util.BoundedText (BoundedText, mkBoundedText)
import Data.Time (UTCTime)
import Data.ULID (ULID)
import Relude
import Text.Regex.PCRE.Heavy (Regex, re, (=~))

-- User

data User = User
  { userId :: ULID,
    userUserName :: UserName,
    userEmail :: Email,
    userHashedPassword :: HashedPassword,
    userCreatedAt :: UTCTime,
    userUpdatedAt :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic)

mkUser :: ULID -> UserName -> Email -> HashedPassword -> UTCTime -> Either UserError User
mkUser userId userName email hashedPassword createdAt =
  Right $ User userId userName email hashedPassword createdAt Nothing

-- Token

newtype Token = Token {unToken :: Text}
  deriving (Show, Eq, Generic)

-- AuthorizedUser

data AuthorizedUser = AuthorizedUser
  { authorizedUserId :: ULID,
    authorizedUserUsername :: UserName,
    authorizedUserEmail :: Email,
    authorizedUserToken :: Token,
    authorizedUserCreatedAt :: UTCTime,
    authorizedUserUpdatedAt :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic)

mkAuthorizedUser :: User -> Token -> AuthorizedUser
mkAuthorizedUser User {userId, userUserName, userEmail, userCreatedAt, userUpdatedAt} token =
  AuthorizedUser
    { authorizedUserId = userId,
      authorizedUserUsername = userUserName,
      authorizedUserEmail = userEmail,
      authorizedUserToken = token,
      authorizedUserCreatedAt = userCreatedAt,
      authorizedUserUpdatedAt = userUpdatedAt
    }

-- UserName

newtype UserName = UserName {unUserName :: BoundedText 3 128}
  deriving (Show, Eq, Generic)

mkUserName :: Text -> Either UserError UserName
mkUserName userName =
  UserName <$> maybeToRight UserNameInvalid (mkBoundedText userName)

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

-- RegisterCommand
data RegisterCommand = RegisterCommand
  { registerCommandUserName :: UserName,
    registerCommandEmail :: Email,
    registerCommandPassword :: Password
  }
  deriving (Show, Eq, Generic)

mkRegisterCommand :: Text -> Text -> Text -> Either UserError RegisterCommand
mkRegisterCommand userName email password = do
  userName' <- mkUserName userName
  email' <- mkEmail email
  password' <- mkPassword password
  pure $ RegisterCommand userName' email' password'