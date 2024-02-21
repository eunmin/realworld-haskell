{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Database.PGArticleRepository where

import Control.Error (headMay)
import Control.Monad.Catch (MonadMask)
import Data.Has (Has)
import Data.ULID (ULID)
import Database.PostgreSQL.Simple (Only (Only), execute, query)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (PGArray (..))
import RealWorld.Domain.Command.Article.Entity.Article (Article (..))
import RealWorld.Domain.Command.Article.Value
import qualified RealWorld.Infra.Component.Database as Database
import RealWorld.Infra.Database.Repo (withConnection)
import Relude

type Database r m = (Has Database.State r, MonadIO m, MonadState r m, MonadMask m, MonadFail m)

deriving instance ToField Slug

deriving instance ToField Title

deriving instance ToField Description

deriving instance ToField Body

deriving instance ToField Tag

instance ToRow Article where
  toRow Article {..} =
    [ toField articleId,
      toField articleSlug,
      toField articleTitle,
      toField articleDescription,
      toField articleBody,
      toField $ PGArray $ toList articleTags,
      toField articleAuthorId,
      toField articleFavoritesCount,
      toField articleCreatedAt,
      -- for on conflict update
      toField articleSlug,
      toField articleTitle,
      toField articleDescription,
      toField articleBody,
      toField $ PGArray $ toList articleTags,
      toField articleAuthorId,
      toField articleFavoritesCount
    ]

deriving instance FromField Slug

deriving instance FromField Title

deriving instance FromField Description

deriving instance FromField Body

deriving instance FromField Tag

instance FromRow Article where
  fromRow =
    Article
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> (fromPGArray <$> field)
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

save :: (Database r m) => Article -> m ()
save user = do
  withConnection $ \conn ->
    liftIO
      $ void
      $ execute
        conn
        "INSERT INTO articles \
        \ (id, slug, title, description, body, tags, author_id, favorites_count, created_at)\
        \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)\
        \ ON CONFLICT (id) DO\
        \ UPDATE SET\
        \   slug = ?,\
        \   title = ?,\
        \   description = ?,\
        \   body = ?,\
        \   tags = ?,\
        \   author_id = ?,\
        \   favorites_count = ?,\
        \   updated_at = now()"
        user

findById :: (Database r m) => ULID -> m (Maybe Article)
findById articleId = do
  withConnection $ \conn ->
    liftIO $ headMay <$> query conn "SELECT * FROM articles WHERE id = ?" (Only articleId)