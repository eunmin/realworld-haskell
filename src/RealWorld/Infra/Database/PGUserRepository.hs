{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Database.PGUserRepository where

import Control.Error (headMay)
import Control.Monad.Catch (MonadMask)
import Data.Has (Has)
import Data.ULID (ULID)
import Database.PostgreSQL.Simple (execute, query)
import Database.PostgreSQL.Simple.FromField
  ( FromField (..),
    ResultError (ConversionFailed, UnexpectedNull),
    returnError,
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (Only (..))
import RealWorld.Domain.User.Entity
  ( Bio (..),
    Email (..),
    HashedPassword (..),
    Image (..),
    User (..),
    Username (..),
  )
import qualified RealWorld.Infra.Component.Database as Database
import RealWorld.Infra.Database.Repo (withConnection)
import RealWorld.Util.BoundedText (BoundedText (..))
import Relude

instance ToRow User where
  toRow User {..} =
    [ toField userId,
      toField userUsername,
      toField userEmail,
      toField userHashedPassword,
      toField userBio,
      toField userImage,
      toField userCreatedAt,
      -- for on conflict update
      toField userUsername,
      toField userEmail,
      toField userHashedPassword,
      toField userBio,
      toField userImage
    ]

instance ToField ULID where
  toField = Escape . show

deriving instance ToField (BoundedText min max)

deriving instance ToField Username

deriving instance ToField Email

deriving instance ToField HashedPassword

deriving instance ToField Bio

deriving instance ToField Image

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromField ULID where
  fromField f mbs =
    case decodeUtf8 <$> mbs of
      Nothing -> returnError UnexpectedNull f ""
      Just dat ->
        case readMaybe dat of
          Nothing -> returnError ConversionFailed f dat
          Just x -> pure x

deriving instance FromField (BoundedText min max)

deriving instance FromField Username

deriving instance FromField Email

deriving instance FromField HashedPassword

deriving instance FromField Bio

deriving instance FromField Image

type Database r m = (Has Database.State r, MonadIO m, MonadState r m, MonadMask m)

save :: (Database r m) => User -> m ()
save user = do
  withConnection $ \conn ->
    liftIO
      $ void
      $ execute
        conn
        "INSERT INTO users (id, username, email, hashed_password, bio, image, created_at)\
        \ VALUES (?, ?, ?, ?, ?, ?, ?)\
        \ ON CONFLICT (id) DO\
        \ UPDATE SET\
        \   username = ?,\
        \   email = ?,\
        \   hashed_password = ?,\
        \   bio = ?,\
        \   image = ?,\
        \   updated_at = now()"
        user

findById :: (Database r m) => ULID -> m (Maybe User)
findById userId = do
  withConnection $ \conn ->
    liftIO $ headMay <$> query conn "SELECT * FROM users WHERE id = ?" (Only userId)

findByUsername :: (Database r m) => Username -> m (Maybe User)
findByUsername (Username (BoundedText username)) = do
  withConnection $ \conn ->
    liftIO $ headMay <$> query conn "SELECT * FROM users WHERE username = ?" (Only username)

findByEmail :: (Database r m) => Email -> m (Maybe User)
findByEmail (Email email) = do
  withConnection $ \conn ->
    liftIO $ headMay <$> query conn "SELECT * FROM users WHERE email = ?" (Only email)