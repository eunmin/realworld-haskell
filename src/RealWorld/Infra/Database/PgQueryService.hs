{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Database.PgQueryService where

import Control.Error (headMay)
import Control.Lens ((.~))
import Data.Generics.Labels ()
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
    FeedArticlesParams (..),
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

type QueryDatabase r m = (Has Database.State r, MonadIO m, MonadReader r m)

deriving anyclass instance FromRow User

deriving anyclass instance FromRow Profile

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
getCurrentUser params = do
  (Database.State pool _) <- asks getter
  liftIO $ withResource pool $ \conn ->
    liftIO
      $ headMay
      <$> query
        conn
        "SELECT email, '', username, bio, image FROM users WHERE id = ?"
        (Only params.actorId)

getProfile :: (QueryDatabase r m) => GetProfileParams -> m (Maybe Profile)
getProfile params = do
  (Database.State pool _) <- asks getter
  liftIO $ withResource pool $ \conn ->
    liftIO
      $ headMay
      <$> query
        conn
        "SELECT username, bio, image, \
        \CASE WHEN f.created_at IS null THEN false ELSE true END following \
        \FROM users u \
        \LEFT JOIN followings f ON u.id = f.following_id AND f.user_id = ? \
        \WHERE username = ?"
        (params.actorId, params.username)

instance ToRow ListArticlesParams where
  toRow params =
    catMaybes
      [ toField <$> params.actorId,
        toField <$> params.actorId,
        toField <$> params.author,
        toField <$> params.tag,
        toField <$> params.favorited,
        toField <$> params.limit,
        toField <$> params.offset
      ]

listArticles :: (QueryDatabase r m) => ListArticlesParams -> m ArticleList
listArticles params = do
  (Database.State pool _) <- asks getter
  liftIO $ withResource pool $ \conn -> do
    let selectSql =
          "SELECT a.slug, a.title, a.description, a.body, a.tags, a.created_at, a.updated_at, "
            <> ( if isJust params.actorId
                   then "CASE WHEN fa.user_id IS null THEN false ELSE true END favorited, "
                   else "false, "
               )
            <> "a.favorites_count, au.username, au.bio, au.image "
            <> ( if isJust params.actorId
                   then ", CASE WHEN fw.created_at IS null THEN false ELSE true END following "
                   else "false "
               )
    let sql =
          "FROM articles a LEFT JOIN users au ON au.id = a.author_id "
            <> actorFavoritedJoinQuery params.actorId
            <> favoritedJoinQuery params.favorited
            <> "WHERE true "
            <> authorWhereQuery params.author
            <> tagWhereQuery params.tag
            <> favoritedWhereQuery params.favorited
    articles <-
      liftIO
        $ query
          conn
          ( selectSql
              <> sql
              <> "ORDER BY a.updated_at DESC LIMIT ? OFFSET ?"
          )
          params
    [Only articlesCount] <-
      liftIO
        $ query
          conn
          ("SELECT count(*) " <> sql)
          (params & #limit .~ Nothing & #offset .~ Nothing)
    pure $ ArticleList articles articlesCount
  where
    actorFavoritedJoinQuery :: Maybe Text -> Query
    actorFavoritedJoinQuery (Just _) =
      "LEFT JOIN favorites fa ON fa.article_id = a.id AND fa.user_id = ? \
      \LEFT JOIN followings fw ON fw.user_id = ? AND fw.following_id = a.author_id "
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

instance ToRow FeedArticlesParams where
  toRow params =
    [ toField params.actorId,
      toField params.limit,
      toField params.offset
    ]

feedArticles :: (QueryDatabase r m) => FeedArticlesParams -> m ArticleList
feedArticles params = do
  (Database.State pool _) <- asks getter
  liftIO $ withResource pool $ \conn -> do
    let selectSql =
          "SELECT a.slug, a.title, a.description, a.body, a.tags, a.created_at, a.updated_at, \
          \ CASE WHEN fa.user_id IS null THEN false ELSE true END favorited, \
          \ a.favorites_count, au.username, au.bio, au.image, true "
    let sql =
          "FROM articles a \
          \ LEFT JOIN users au ON au.id = a.author_id \
          \ LEFT JOIN favorites fa ON fa.article_id = a.id AND fa.user_id = ? \
          \WHERE \
          \ a.author_id in (SELECT following_id FROM followings WHERE user_id = ?) "
    articles <-
      liftIO
        $ query
          conn
          ( selectSql
              <> sql
              <> "ORDER BY a.updated_at DESC LIMIT ? OFFSET ?"
          )
          params
    [Only articlesCount] <-
      liftIO
        $ query
          conn
          ("SELECT count(*) " <> sql)
          (params.actorId, params.actorId)
    pure $ ArticleList articles articlesCount

getArticle :: (QueryDatabase r m) => GetArticleParams -> m (Maybe Article)
getArticle params = do
  (Database.State pool _) <- asks getter
  liftIO $ withResource pool $ \conn ->
    liftIO
      $ headMay
      <$> query
        conn
        "SELECT a.slug, a.title, a.description, a.body, a.tags, a.created_at, a.updated_at, \
        \false, a.favorites_count, u.username, u.bio, u.image, false \
        \FROM articles a \
        \LEFT JOIN users u ON a.author_id = u.id \
        \WHERE a.slug = ?"
        (Only params.slug)

getComments :: (QueryDatabase r m) => GetCommentsParams -> m CommentList
getComments params = do
  (Database.State pool _) <- asks getter
  liftIO $ withResource pool $ \conn -> do
    comments <-
      liftIO
        $ query
          conn
          "SELECT c.id, c.created_at, c.updated_at, c.body, u.username, u.bio, u.image, false \
          \FROM comments c \
          \LEFT JOIN articles a ON a.id = c.article_id \
          \LEFT JOIN users u ON c.author_id = u.id \
          \WHERE a.slug = ?"
          (Only params.slug)
    pure $ CommentList comments

getTags :: (QueryDatabase r m) => m TagList
getTags = do
  (Database.State pool _) <- asks getter
  liftIO $ withResource pool $ \conn -> do
    fromMaybe (TagList [])
      <$> liftIO
        ( headMay
            <$> query
              conn
              "SELECT array_agg(DISTINCT c) FROM (SELECT unnest(tags) FROM articles) AS dt(c)"
              ()
        )
