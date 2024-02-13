{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conduit.Infra.Database.PGUserRepository where

import Conduit.Domain.User.Entity (Email (..), HashedPassword (..), User (..), UserName (..))
import qualified Conduit.Infra.Component.Database as Database
-- import Data.Pool (withResource)

import Conduit.Infra.Database.Repo (withConnection)
import Conduit.Util.BoundedText (BoundedText (..))
import Conduit.Util.Pool (withResource)
import Control.Error (headMay)
import Control.Monad.Catch
import Data.Has (Has (getter))
import Data.ULID (ULID)
import Database.PostgreSQL.Simple (Connection, execute, query)
import Database.PostgreSQL.Simple.FromField
  ( FromField (..),
    ResultError (ConversionFailed, UnexpectedNull),
    returnError,
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (Only (..))
import Relude

instance ToRow User where
  toRow User {..} =
    [ toField userId,
      toField userUserName,
      toField userEmail,
      toField userHashedPassword,
      toField userCreatedAt
    ]

instance ToField ULID where
  toField = Escape . show

deriving instance ToField (BoundedText min max)

deriving instance ToField UserName

deriving instance ToField Email

deriving instance ToField HashedPassword

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance FromField ULID where
  fromField f mbs =
    case decodeUtf8 <$> mbs of
      Nothing -> returnError UnexpectedNull f ""
      Just dat ->
        case readMaybe dat of
          Nothing -> returnError ConversionFailed f dat
          Just x -> pure x

deriving instance FromField (BoundedText min max)

deriving instance FromField UserName

deriving instance FromField Email

deriving instance FromField HashedPassword

type Database r m = (Has Database.State r, MonadIO m, MonadState r m, MonadMask m)

create :: (Database r m) => User -> m ()
create user = do
  withConnection $ \conn ->
    liftIO
      $ void
      $ execute
        conn
        "INSERT INTO users (id, username, email, hashed_password, created_at)\
        \ VALUES (?, ?, ?, ?, ?)"
        user

findById :: (Database r m) => ULID -> m (Maybe User)
findById userId = do
  withConnection $ \conn ->
    liftIO $ headMay <$> query conn "SELECT * FROM users WHERE id = ?" (Only userId)

findByUsername :: (Database r m) => UserName -> m (Maybe User)
findByUsername (UserName (BoundedText userName)) = do
  withConnection $ \conn ->
    liftIO $ headMay <$> query conn "SELECT * FROM users WHERE username = ?" (Only userName)

findByEmail :: (Database r m) => Email -> m (Maybe User)
findByEmail (Email email) = do
  withConnection $ \conn ->
    liftIO $ headMay <$> query conn "SELECT * FROM users WHERE email = ?" (Only email)