{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Repository.PgQuery where

import Control.Error (headMay)
import Data.Has (Has (..))
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (Only (Only), Query, ToRow, query)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (PGArray (..))
import RealWorld.Domain.Query.Data
  ( Article (Article),
    ArticleList (..),
    Comment (..),
    CommentList (..),
    FeedArticlesParams,
    GetArticleParams (..),
    GetCommentsParams (..),
    GetCurrentUserParams (..),
    GetProfileParams (..),
    ListArticlesParams (..),
    Profile (..),
    TagList (..),
    User,
  )
import qualified RealWorld.Infra.Component.Database as Database
import Relude

type QueryDatabase r m = (Has Database.State r, MonadIO m, MonadState r m)

deriving instance FromRow User

deriving instance FromRow Profile

instance FromRow Article where
  fromRow =
    Article
      <$> field
      <*> field
      <*> field
      <*> field
      <*> (fromPGArray <$> field)
      <*> field
      <*> field
      <*> field
      <*> field
      <*> (Profile <$> field <*> field <*> field <*> field)

instance FromRow Comment where
  fromRow =
    Comment
      <$> field
      <*> field
      <*> field
      <*> field
      <*> (Profile <$> field <*> field <*> field <*> field)

instance FromRow TagList where
  fromRow = TagList <$> (fromPGArray <$> field)

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

instance ToRow ListArticlesParams where
  toRow ListArticlesParams {..} =
    catMaybes
      [ toField <$> listArticlesParamsActorId,
        toField <$> listArticlesParamsAuthor,
        toField <$> listArticlesParamsTag,
        toField <$> listArticlesParamsFavorited,
        pure (toField listArticlesParamsLimit),
        pure (toField listArticlesParamsOffset)
      ]

listArticles :: (QueryDatabase r m) => ListArticlesParams -> m ArticleList
listArticles params@ListArticlesParams {..} = do
  (Database.State pool _) <- gets getter
  liftIO $ withResource pool $ \conn -> do
    articles <-
      liftIO $
        query
          conn
          ( "SELECT a.slug, a.title, a.description, a.body, a.tags, a.created_at, a.updated_at, \
            \false, a.favorites_count, au.username, au.bio, au.image, false \
            \FROM articles a LEFT JOIN users au ON au.id = a.author_id "
              <> actorFavoritedJoinQuery listArticlesParamsActorId
              <> favoritedJoinQuery listArticlesParamsFavorited
              <> "WHERE true "
              <> authorWhereQuery listArticlesParamsAuthor
              <> tagWhereQuery listArticlesParamsTag
              <> favoritedWhereQuery listArticlesParamsFavorited
              <> "ORDER BY a.updated_at DESC LIMIT ? OFFSET ?"
          )
          params
    pure $ ArticleList articles 0
  where
    actorFavoritedJoinQuery :: Maybe Text -> Query
    actorFavoritedJoinQuery (Just _) =
      "LEFT JOIN users aa ON aa.username = ? \
      \LEFT JOIN favorites fa ON fa.article_id = a.id AND fa.user_id = aa.id "
    actorFavoritedJoinQuery Nothing = ""
    favoritedJoinQuery :: Maybe Text -> Query
    favoritedJoinQuery (Just _) =
      "LEFT JOIN favorites f ON f.article_id = a.id \
      \ LEFT JOIN users fu ON fu.id = f.user_id "
    favoritedJoinQuery Nothing = ""
    authorWhereQuery :: Maybe Text -> Query
    authorWhereQuery (Just _) = "AND au.username = ? "
    authorWhereQuery Nothing = ""
    tagWhereQuery :: Maybe Text -> Query
    tagWhereQuery (Just _) = "AND ? = ANY(a.tags) "
    tagWhereQuery Nothing = ""
    favoritedWhereQuery :: Maybe Text -> Query
    favoritedWhereQuery (Just _) = "AND fu.username = ? "
    favoritedWhereQuery Nothing = ""

feedArticles :: (QueryDatabase r m) => FeedArticlesParams -> m ArticleList
feedArticles = undefined

getArticle :: (QueryDatabase r m) => GetArticleParams -> m (Maybe Article)
getArticle GetArticleParams {..} = do
  (Database.State pool _) <- gets getter
  liftIO $ withResource pool $ \conn ->
    liftIO $
      headMay
        <$> query
          conn
          "SELECT a.slug, a.title, a.description, a.body, a.tags, a.created_at, a.updated_at, \
          \false, a.favorites_count, u.username, u.bio, u.image, false \
          \FROM articles a \
          \LEFT JOIN users u ON a.author_id = u.id \
          \WHERE a.slug = ?"
          (Only getArticleParamsSlug)

getComments :: (QueryDatabase r m) => GetCommentsParams -> m CommentList
getComments GetCommentsParams {..} = do
  (Database.State pool _) <- gets getter
  liftIO $ withResource pool $ \conn -> do
    comments <-
      liftIO $
        query
          conn
          "SELECT c.id, c.created_at, c.updated_at, c.body, u.username, u.bio, u.image, false \
          \FROM comments c \
          \LEFT JOIN articles a ON a.id = c.article_id \
          \LEFT JOIN users u ON c.author_id = u.id \
          \WHERE a.slug = ?"
          (Only getCommentsParamsSlug)
    pure $ CommentList comments

getTags :: (QueryDatabase r m) => m TagList
getTags = do
  (Database.State pool _) <- gets getter
  liftIO $ withResource pool $ \conn -> do
    fromMaybe (TagList [])
      <$> liftIO
        ( headMay
            <$> query
              conn
              "SELECT array_agg(DISTINCT c) FROM (SELECT unnest(tags) FROM articles) AS dt(c)"
              ()
        )