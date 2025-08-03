{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module RealWorld.Domain.Command.User.Value where

import Data.Aeson (ToJSON)
import RealWorld.Domain.Util.BoundedText (BoundedText, mkBoundedText)
import Relude
import Text.Regex.PCRE.Heavy (Regex, re, (=~))

----------------------------------------------------------------------------------------------------
-- Token

newtype Token = Token {unToken :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON)

tokeExpiresInSec :: Int
tokeExpiresInSec = 60 * 60 * 24 -- 1 day

----------------------------------------------------------------------------------------------------
-- Username

newtype Username = Username {unUsername :: BoundedText 3 128}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON)

mkUsername :: Text -> Maybe Username
mkUsername username =
  Username <$> mkBoundedText username

----------------------------------------------------------------------------------------------------
-- Email

newtype Email = Email {unEmail :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON)

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
  deriving stock (Show, Eq, Generic)

passwordRegex :: Regex
passwordRegex = [re|^(?=.*[A-Za-z])(?=.*\d)(?=.*[@$!%*#?&])[A-Za-z\d@$!%*#?&]{12,}$|]

mkPassword :: Text -> Maybe Password
mkPassword password =
  if password =~ passwordRegex
    then Just $ Password password
    else Nothing

newtype HashedPassword = HashedPassword {unHashedPassword :: Text}
  deriving stock (Show, Eq, Generic)

----------------------------------------------------------------------------------------------------
-- Bio
newtype Bio = Bio {unBio :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON)

mkBio :: Text -> Bio
mkBio = Bio

emptyBio :: Bio
emptyBio = Bio ""

----------------------------------------------------------------------------------------------------
-- Image

newtype Image = Image {unImage :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON)

mkImage :: Maybe Text -> Maybe Image
mkImage image = Image <$> image
