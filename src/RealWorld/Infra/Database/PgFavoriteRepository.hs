{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Database.PgFavoriteRepository where

import Control.Exception.Safe (MonadMask)
import Data.Has (Has)
import Database.PostgreSQL.Simple (
  FromRow,
  ToRow,
  execute,
  query,
 )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import RealWorld.Domain.Command.Article.Entity.Favorite (Favorite (..))
import RealWorld.Domain.Command.Article.Value (FavoriteId (..))
import RealWorld.Infra.Component.Database (withConnection)
import RealWorld.Infra.Component.Database qualified as Database
import RealWorld.Infra.Converter.PostgreSQL ()
import Safe (headMay)

type Database r m = (Has Database.State r, MonadIO m, MonadReader r m, MonadMask m, MonadFail m)

instance ToRow Favorite where
  toRow Favorite {..} =
    [ toField favoriteId.articleId
    , toField favoriteId.userId
    , toField createdAt
    ]

instance FromRow Favorite where
  fromRow =
    Favorite
      <$> (FavoriteId <$> field <*> field)
      <*> field

save :: (Database r m) => Favorite -> m Bool
save favorite =
  withConnection $ \conn ->
    liftIO $
      (> 0)
        <$> execute
          conn
          "INSERT INTO favorites \
          \ (article_id, user_id, created_at)\
          \ VALUES (?, ?, ?)\
          \ ON CONFLICT (article_id, user_id) DO NOTHING"
          favorite

findById :: (Database r m) => FavoriteId -> m (Maybe Favorite)
findById (FavoriteId articleId userId) =
  withConnection $ \conn ->
    liftIO $
      headMay
        <$> query
          conn
          "SELECT article_id, user_id,  created_at \
          \FROM favorites WHERE article_id = ? AND user_id = ?"
          (articleId, userId)

delete :: (Database r m) => Favorite -> m Bool
delete favorite = do
  let FavoriteId articleId userId = favorite.favoriteId
  withConnection $ \conn ->
    liftIO $
      (> 0)
        <$> execute
          conn
          "DELETE FROM favorites WHERE article_id = ? AND user_id = ?"
          (articleId, userId)
