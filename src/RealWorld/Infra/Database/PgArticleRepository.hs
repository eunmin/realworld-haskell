{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Database.PgArticleRepository where

import Control.Error (headMay)
import Control.Monad.Catch (MonadMask)
import Data.Has (Has)
import Data.ULID (ULID)
import Database.PostgreSQL.Simple
  ( FromRow,
    Only (Only),
    ToRow,
    execute,
    query,
  )
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (PGArray (..))
import RealWorld.Domain.Command.Article.Entity.Article (Article (..))
import RealWorld.Domain.Command.Article.Value
  ( ArticleBody (ArticleBody),
    Description (Description),
    Slug (..),
    Tag (Tag),
    Title (Title),
  )
import RealWorld.Infra.Component.Database (withConnection)
import qualified RealWorld.Infra.Component.Database as Database
import RealWorld.Infra.Converter.PostgreSQL ()
import Relude

type Database r m = (Has Database.State r, MonadIO m, MonadState r m, MonadMask m, MonadFail m)

deriving instance ToField Slug

deriving instance ToField Title

deriving instance ToField Description

deriving instance ToField ArticleBody

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

deriving instance FromField ArticleBody

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

save :: (Database r m) => Article -> m Bool
save user =
  withConnection $ \conn ->
    liftIO
      $ (> 0)
      <$> execute
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
findById articleId =
  withConnection $ \conn ->
    liftIO
      $ headMay
      <$> query
        conn
        "SELECT id, slug, title, description, body, tags, created_at, updated_at, false,\
        \       favorites_count, author_id \
        \FROM articles WHERE id = ?"
        (Only articleId)

findBySlug :: (Database r m) => Slug -> m (Maybe Article)
findBySlug slug =
  withConnection $ \conn ->
    liftIO
      $ headMay
      <$> query
        conn
        "SELECT id, slug, title, description, body, tags, created_at, updated_at, false,\
        \       favorites_count, author_id \
        \FROM articles WHERE slug = ?"
        (Only $ unSlug slug)

delete :: (Database r m) => Article -> m Bool
delete article =
  withConnection $ \conn ->
    liftIO $ (> 0) <$> execute conn "DELETE FROM articles WHERE id = ?" (Only $ articleId article)