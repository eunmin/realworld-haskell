{-# LANGUAGE DeriveAnyClass #-}

module RealWorld.Domain.Query.Data where

import Data.Aeson
import Data.Time (UTCTime)
import Relude

data GetCurrentUserParams = GetCurrentUserParams
  { actorId :: Text
  }
  deriving stock (Show, Eq, Generic)

data GetProfileParams = GetProfileParams
  { actorId :: Maybe Text
  , username :: Text
  }
  deriving stock (Show, Eq, Generic)

data ListArticlesParams = ListArticlesParams
  { actorId :: Maybe Text
  , tag :: Maybe Text
  , author :: Maybe Text
  , favorited :: Maybe Text
  , limit :: Maybe Int
  , offset :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

data FeedArticlesParams = FeedArticlesParams
  { actorId :: Text
  , limit :: Int
  , offset :: Int
  }
  deriving stock (Show, Eq, Generic)

data GetArticleParams = GetArticleParams
  { slug :: Text
  }
  deriving stock (Show, Eq, Generic)

data GetCommentsParams = GetCommentsParams
  { actorId :: Maybe Text
  , slug :: Text
  }
  deriving stock (Show, Eq, Generic)

data User = User
  { email :: Text
  , token :: Text
  , username :: Text
  , bio :: Text
  , image :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Profile = Profile
  { username :: Text
  , bio :: Text
  , image :: Maybe Text
  , following :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

userToProfile :: User -> Bool -> Profile
userToProfile user following =
  Profile
    { username = user.username
    , bio = user.bio
    , image = user.image
    , following = following
    }

data Article = Article
  { slug :: Text
  , title :: Text
  , description :: Text
  , body :: Text
  , tagList :: [Text]
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  , favorited :: Bool
  , favoritesCount :: Int
  , author :: Profile
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ArticleList = ArticleList
  { articles :: [Article]
  , articlesCount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Comment = Comment
  { commentId :: Text
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  , body :: Text
  , author :: Profile
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CommentList = CommentList
  { comments :: [Comment]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TagList = TagList
  { tags :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
