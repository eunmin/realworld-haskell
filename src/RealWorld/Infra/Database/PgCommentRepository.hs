{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Database.PgCommentRepository where

import Data.ULID (ULID)
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
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Dynamic (Reader)
import RealWorld.Domain.Command.Article.Entity.Comment
import RealWorld.Domain.Command.Article.Value (CommentBody (..))
import RealWorld.Infra.Component.Database (withConnection)
import qualified RealWorld.Infra.Component.Database as Database
import RealWorld.Infra.Converter.PostgreSQL ()
import Relude hiding (Reader)
import Safe (headMay)

type Database es = (Reader Database.State :> es, IOE :> es)

deriving newtype instance ToField CommentBody

instance ToRow Comment where
  toRow Comment{..} =
    [ toField commentId
    , toField body
    , toField createdAt
    , toField updatedAt
    , toField authorId
    , toField articleId
    , -- for on conflict update
      toField body
    , toField createdAt
    , toField authorId
    , toField articleId
    ]

deriving newtype instance FromField CommentBody

instance FromRow Comment where
  fromRow =
    Comment
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

save :: (Database es) => Comment -> Eff es Bool
save comment =
  withConnection $ \conn ->
    liftIO $
      (> 0)
        <$> execute
          conn
          "INSERT INTO comments \
          \ (id, body, created_at, updated_at, author_id, article_id)\
          \ VALUES (?, ?, ?, ?, ?, ?)\
          \ ON CONFLICT (id) DO\
          \ UPDATE SET\
          \   body = ?,\
          \   created_at = ?,\
          \   author_id = ?,\
          \   article_id = ?,\
          \   updated_at = now()"
          comment

findById :: (Database es) => ULID -> Eff es (Maybe Comment)
findById commentId =
  withConnection $ \conn ->
    liftIO $
      headMay
        <$> query
          conn
          "SELECT id, body, created_at, updated_at, author_id, article_id \
          \FROM comments WHERE id = ?"
          (Only commentId)

delete :: (Database es) => Comment -> Eff es Bool
delete comment =
  withConnection $ \conn ->
    liftIO $ (> 0) <$> execute conn "DELETE FROM comments WHERE id = ?" (Only comment.commentId)
