{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Repository.PgQuery where

import Control.Error (headMay)
import Data.Has (Has (..))
import Data.Pool (withResource)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow)
import RealWorld.Domain.Query.Data
import qualified RealWorld.Infra.Component.Database as Database
import Relude

type QueryDatabase r m = (Has Database.State r, MonadIO m, MonadState r m)

deriving instance FromRow User

deriving instance FromRow Profile

getCurrentUser :: (QueryDatabase r m) => GetCurrentUserParams -> m (Maybe User)
getCurrentUser GetCurrentUserParams {..} = do
  (Database.State pool _) <- gets getter
  liftIO $ withResource pool $ \conn ->
    liftIO $
      headMay
        <$> query
          conn
          "SELECT email, '', username, bio, image FROM users WHERE id = ?"
          (Only getCurrentUserParamsActorId)

getProfile :: (QueryDatabase r m) => GetProfileParams -> m (Maybe Profile)
getProfile GetProfileParams {..} = do
  (Database.State pool _) <- gets getter
  liftIO $ withResource pool $ \conn ->
    liftIO $
      headMay
        <$> query
          conn
          "SELECT username, bio, image, \
          \CASE WHEN f.created_at IS null THEN false ELSE true END following \
          \FROM users u \
          \LEFT JOIN followings f ON u.id = f.following_id AND f.user_id = ? \
          \WHERE username = ?"
          (getProfileParamsActorId, getProfileParamsUsername)

listArticles :: ListArticlesParams -> m ArticleList
listArticles = undefined

feedArticles :: FeedArticlesParams -> m ArticleList
feedArticles = undefined

getArticle :: GetArticleParams -> m (Maybe Article)
getArticle = undefined

getCommentsFromArticle :: GetCommentsFromArticleParams -> m CommentList
getCommentsFromArticle = undefined

getTags :: m TagList
getTags = undefined
