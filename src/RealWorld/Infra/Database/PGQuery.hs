{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Database.PGQuery where

import Control.Error (headMay)
import Data.Has (Has (..))
import Data.Pool (withResource)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import qualified RealWorld.Infra.Component.Database as Database
import RealWorld.Query.Types
import Relude

type QueryDatabase r m = (Has Database.State r, MonadIO m, MonadState r m)

deriving instance FromRow User

deriving instance FromRow Profile

getCurrentUser :: (QueryDatabase r m) => GetCurrentUserParams -> m (Maybe User)
getCurrentUser GetCurrentUserParams {..} = do
  (Database.State pool _) <- gets getter
  liftIO $ withResource pool $ \conn ->
    liftIO
      $ headMay
      <$> query
        conn
        "SELECT email, '', username, bio, image FROM users WHERE id = ?"
        (Only sessionUserId)

getProfile :: (QueryDatabase r m) => GetProfileParams -> m (Maybe Profile)
getProfile GetProfileParams {..} = do
  (Database.State pool _) <- gets getter
  liftIO $ withResource pool $ \conn ->
    liftIO
      $ headMay
      <$> query
        conn
        "SELECT username, bio, image, false FROM users WHERE username = ?"
        (Only username)

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
