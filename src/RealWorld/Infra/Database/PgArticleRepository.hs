{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Database.PgArticleRepository where

import Control.Error (headMay)
import Database.PostgreSQL.Simple (
  FromRow,
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
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic (Reader)
import RealWorld.Domain.Command.Article.Entity.Article (Article (..))
import RealWorld.Domain.Command.Article.Value (
  ArticleBody (ArticleBody),
  Description (Description),
  Slug (..),
  Tag (Tag),
  Title (Title),
 )
import RealWorld.Infra.Component.Database (withConnection)
import qualified RealWorld.Infra.Component.Database as Database
import RealWorld.Infra.Converter.PostgreSQL ()
import Relude hiding (Reader)

type Database es = (Reader Database.State :> es, IOE :> es)

deriving newtype instance ToField Slug

deriving newtype instance ToField Title

deriving newtype instance ToField Description

deriving newtype instance ToField ArticleBody

deriving newtype instance ToField Tag

instance ToRow Article where
  toRow Article{..} =
    [ toField articleId
    , toField slug
    , toField title
    , toField description
    , toField body
    , toField $ PGArray $ toList tags
    , toField authorId
    , toField favoritesCount
    , toField createdAt
    , -- for on conflict update
      toField slug
    , toField title
    , toField description
    , toField body
    , toField $ PGArray $ toList tags
    , toField authorId
    , toField favoritesCount
    ]

deriving newtype instance FromField Slug

deriving newtype instance FromField Title

deriving newtype instance FromField Description

deriving newtype instance FromField ArticleBody

deriving newtype instance FromField Tag

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

save :: (Database es) => Article -> Eff es Bool
save user =
  withConnection $ \conn ->
    liftIO $
      (> 0)
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

findBySlug :: (Database es) => Slug -> Eff es (Maybe Article)
findBySlug slug =
  withConnection $ \conn ->
    liftIO $
      headMay
        <$> query
          conn
          "SELECT id, slug, title, description, body, tags, created_at, updated_at, false,\
          \       favorites_count, author_id \
          \FROM articles WHERE slug = ?"
          (Only $ unSlug slug)

delete :: (Database es) => Article -> Eff es Bool
delete article =
  withConnection $ \conn ->
    liftIO $ (> 0) <$> execute conn "DELETE FROM articles WHERE id = ?" (Only article.articleId)
