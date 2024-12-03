{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Database.PgUserRepository where

import Control.Error (headMay)
import Control.Monad.Catch (MonadMask)
import Data.Has (Has)
import Data.ULID (ULID)
import Database.PostgreSQL.Simple (execute, query)
import Database.PostgreSQL.Simple.FromField (
  FromField (..),
 )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (Only (..))
import RealWorld.Domain.Command.User.Entity.User (
  User (..),
 )
import RealWorld.Domain.Command.User.Value (
  Bio (..),
  Email (..),
  HashedPassword (..),
  Image (..),
  Username (..),
 )
import RealWorld.Domain.Util.BoundedText (BoundedText (..))
import RealWorld.Infra.Component.Database (withConnection)
import RealWorld.Infra.Component.Database qualified as Database
import RealWorld.Infra.Converter.PostgreSQL ()

instance ToRow User where
  toRow User {..} =
    [ toField userId
    , toField userUsername
    , toField userEmail
    , toField userHashedPassword
    , toField userBio
    , toField userImage
    , toField userCreatedAt
    , -- for on conflict update
      toField userUsername
    , toField userEmail
    , toField userHashedPassword
    , toField userBio
    , toField userImage
    ]

deriving newtype instance ToField (BoundedText min max)

deriving newtype instance ToField Username

deriving newtype instance ToField Email

deriving newtype instance ToField HashedPassword

deriving newtype instance ToField Bio

deriving newtype instance ToField Image

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

deriving newtype instance FromField (BoundedText min max)

deriving newtype instance FromField Username

deriving newtype instance FromField Email

deriving newtype instance FromField HashedPassword

deriving newtype instance FromField Bio

deriving newtype instance FromField Image

type Database r m = (Has Database.State r, MonadIO m, MonadReader r m, MonadMask m, MonadFail m)

save :: (Database r m) => User -> m Bool
save user = do
  withConnection $ \conn ->
    liftIO $
      (> 0)
        <$> execute
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

follow :: (Database r m) => ULID -> ULID -> m Bool
follow followerId followeeId = do
  withConnection $ \conn ->
    liftIO $
      (> 0)
        <$> execute
          conn
          "INSERT INTO followings (user_id, following_id) VALUES (?, ?)"
          (followerId, followeeId)

unfollow :: (Database r m) => ULID -> ULID -> m Bool
unfollow followerId followeeId = do
  withConnection $ \conn ->
    liftIO $
      (> 0)
        <$> execute
          conn
          "DELETE FROM followings WHERE user_id = ? AND following_id = ?"
          (followerId, followeeId)

hasFollowing :: (Database r m) => ULID -> ULID -> m Bool
hasFollowing followerId followeeId = do
  withConnection $ \conn -> do
    [Only (count :: Int)] <-
      liftIO $
        query
          conn
          "SELECT count(*) FROM followings WHERE user_id = ? AND following_id = ?"
          (followerId, followeeId)
    pure $ count > 0
