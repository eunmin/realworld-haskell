{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module RealWorld.Query.Types where

import Data.Aeson
import Relude

data GetCurrentUserParams = GetCurrentUserParams
  { sessionUserId :: Text
  }
  deriving (Show, Eq)

data GetProfileParams = GetProfileParams
  { sessionUserId :: Maybe Text,
    username :: Text
  }
  deriving (Show, Eq)

data ListArticlesParams = ListArticlesParams
  { sessionUserId :: Maybe Text,
    tag :: Maybe Text,
    author :: Maybe Text,
    favorited :: Maybe Text,
    limit :: Maybe Int,
    offset :: Maybe Int
  }
  deriving (Show, Eq)

data FeedArticlesParams = FeedArticlesParams
  { sessionUserId :: Text,
    limit :: Maybe Int,
    offset :: Maybe Int
  }
  deriving (Show, Eq)

data GetArticleParams = GetArticleParams
  { sessionUserId :: Maybe Text,
    slug :: Text
  }
  deriving (Show, Eq)

data GetCommentsFromArticleParams = GetCommentsFromArticleParams
  { sessionUserId :: Maybe Text,
    slug :: Text
  }
  deriving (Show, Eq)

class Query m where
  getCurrentUser :: GetCurrentUserParams -> m (Maybe User)
  getProfile :: GetProfileParams -> m (Maybe Profile)
  listArticles :: ListArticlesParams -> m ArticleList
  feedArticles :: FeedArticlesParams -> m ArticleList
  getArticle :: GetArticleParams -> m (Maybe Article)
  getCommentsFromArticle :: GetCommentsFromArticleParams -> m CommentList
  getTags :: m TagList

data User = User
  { email :: Text,
    token :: Text,
    username :: Text,
    bio :: Text,
    image :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON)

data Profile = Profile
  { username :: Text,
    bio :: Text,
    image :: Maybe Text,
    following :: Bool
  }
  deriving (Generic, Show, Eq, ToJSON)

data Article = Article
  { slug :: Text,
    title :: Text,
    description :: Text,
    body :: Text,
    tagList :: [Text],
    createdAt :: Text,
    updatedAt :: Text,
    favorited :: Bool,
    favoritesCount :: Int,
    author :: Profile
  }
  deriving (Generic, Show, Eq, ToJSON)

data ArticleList = ArticleList
  { articles :: [Article],
    articlesCount :: Int
  }
  deriving (Generic, Show, Eq, ToJSON)

data Comment = Comment
  { id :: Int,
    createdAt :: Text,
    updatedAt :: Text,
    body :: Text,
    author :: Profile
  }
  deriving (Generic, Show, Eq, ToJSON)

data CommentList = CommentList
  { comments :: [Comment]
  }
  deriving (Generic, Show, Eq, ToJSON)

data TagList = TagList
  { tags :: [Text]
  }
  deriving (Generic, Show, Eq, ToJSON)
