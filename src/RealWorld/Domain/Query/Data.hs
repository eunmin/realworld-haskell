{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DuplicateRecordFields #-}

module RealWorld.Domain.Query.Data where

import Data.Time (UTCTime)

data GetCurrentUserParams = GetCurrentUserParams
  { getCurrentUserParamsActorId :: Text
  }
  deriving stock (Show, Eq)

data GetProfileParams = GetProfileParams
  { getProfileParamsActorId :: Maybe Text
  , getProfileParamsUsername :: Text
  }
  deriving stock (Show, Eq)

data ListArticlesParams = ListArticlesParams
  { listArticlesParamsActorId :: Maybe Text
  , listArticlesParamsTag :: Maybe Text
  , listArticlesParamsAuthor :: Maybe Text
  , listArticlesParamsFavorited :: Maybe Text
  , listArticlesParamsLimit :: Maybe Int
  , listArticlesParamsOffset :: Maybe Int
  }
  deriving stock (Show, Eq)

data FeedArticlesParams = FeedArticlesParams
  { feedArticlesParamsActorId :: Text
  , feedArticlesParamsLimit :: Int
  , feedArticlesParamsOffset :: Int
  }
  deriving stock (Show, Eq)

data GetArticleParams = GetArticleParams
  { getArticleParamsSlug :: Text
  }
  deriving stock (Show, Eq)

data GetCommentsParams = GetCommentsParams
  { getCommentsParamsActorId :: Maybe Text
  , getCommentsParamsSlug :: Text
  }
  deriving stock (Show, Eq)

data User = User
  { userEmail :: Text
  , userToken :: Text
  , userUsername :: Text
  , userBio :: Text
  , userImage :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)

data Profile = Profile
  { profileUsername :: Text
  , profileBio :: Text
  , profileImage :: Maybe Text
  , profileFollowing :: Bool
  }
  deriving stock (Generic, Show, Eq)

userToProfile :: User -> Bool -> Profile
userToProfile User{..} following =
  Profile
    { profileUsername = userUsername
    , profileBio = userBio
    , profileImage = userImage
    , profileFollowing = following
    }

data Article = Article
  { articleSlug :: Text
  , articleTitle :: Text
  , articleDescription :: Text
  , articleBody :: Text
  , articleTagList :: [Text]
  , articleCreatedAt :: UTCTime
  , articleUpdatedAt :: Maybe UTCTime
  , articleFavorited :: Bool
  , articleFavoritesCount :: Int
  , articleAuthor :: Profile
  }
  deriving stock (Generic, Show, Eq)

data ArticleList = ArticleList
  { articleListArticles :: [Article]
  , articleListArticlesCount :: Int
  }
  deriving stock (Generic, Show, Eq)

data Comment = Comment
  { commentId :: Text
  , commentCreatedAt :: UTCTime
  , commentUpdatedAt :: Maybe UTCTime
  , commentBody :: Text
  , commentAuthor :: Profile
  }
  deriving stock (Generic, Show, Eq)

data CommentList = CommentList
  { commentListComments :: [Comment]
  }
  deriving stock (Generic, Show, Eq)

data TagList = TagList
  { tagListTags :: [Text]
  }
  deriving stock (Generic, Show, Eq)