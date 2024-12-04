{-# LANGUAGE NoFieldSelectors #-}

module RealWorld.Domain.Query.Data where

import Data.Time (UTCTime)

data GetCurrentUserParams = GetCurrentUserParams
  { actorId :: Text
  }
  deriving stock (Show, Eq)

data GetProfileParams = GetProfileParams
  { actorId :: Maybe Text
  , username :: Text
  }
  deriving stock (Show, Eq)

data ListArticlesParams = ListArticlesParams
  { actorId :: Maybe Text
  , tag :: Maybe Text
  , author :: Maybe Text
  , favorited :: Maybe Text
  , limit :: Maybe Int
  , offset :: Maybe Int
  }
  deriving stock (Show, Eq)

data FeedArticlesParams = FeedArticlesParams
  { actorId :: Text
  , limit :: Int
  , offset :: Int
  }
  deriving stock (Show, Eq)

data GetArticleParams = GetArticleParams
  { slug :: Text
  }
  deriving stock (Show, Eq)

data GetCommentsParams = GetCommentsParams
  { actorId :: Maybe Text
  , slug :: Text
  }
  deriving stock (Show, Eq)

data User = User
  { email :: Text
  , token :: Text
  , username :: Text
  , bio :: Text
  , image :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)

data Profile = Profile
  { username :: Text
  , bio :: Text
  , image :: Maybe Text
  , following :: Bool
  }
  deriving stock (Generic, Show, Eq)

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
  deriving stock (Generic, Show, Eq)

data ArticleList = ArticleList
  { articles :: [Article]
  , articlesCount :: Int
  }
  deriving stock (Generic, Show, Eq)

data Comment = Comment
  { commentId :: Text
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  , body :: Text
  , author :: Profile
  }
  deriving stock (Generic, Show, Eq)

data CommentList = CommentList
  { comments :: [Comment]
  }
  deriving stock (Generic, Show, Eq)

data TagList = TagList
  { tags :: [Text]
  }
  deriving stock (Generic, Show, Eq)
