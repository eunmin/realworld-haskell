{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DuplicateRecordFields #-}

module RealWorld.Domain.Query.Data where

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

data User = User
  { userEmail :: Text,
    userToken :: Text,
    userUsername :: Text,
    userBio :: Text,
    userImage :: Maybe Text
  }
  deriving (Generic, Show, Eq)

data Profile = Profile
  { profileUsername :: Text,
    profileBio :: Text,
    profileImage :: Maybe Text,
    profileFollowing :: Bool
  }
  deriving (Generic, Show, Eq)

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
  deriving (Generic, Show, Eq)

data ArticleList = ArticleList
  { articles :: [Article],
    articlesCount :: Int
  }
  deriving (Generic, Show, Eq)

data Comment = Comment
  { id :: Int,
    createdAt :: Text,
    updatedAt :: Text,
    body :: Text,
    author :: Profile
  }
  deriving (Generic, Show, Eq)

data CommentList = CommentList
  { comments :: [Comment]
  }
  deriving (Generic, Show, Eq)

data TagList = TagList
  { tags :: [Text]
  }
  deriving (Generic, Show, Eq)