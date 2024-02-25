{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DuplicateRecordFields #-}

module RealWorld.Domain.Query.Data where

import Data.Time (UTCTime)
import Relude

data GetCurrentUserParams = GetCurrentUserParams
  { getCurrentUserParamsActorId :: Text
  }
  deriving (Show, Eq)

data GetProfileParams = GetProfileParams
  { getProfileParamsActorId :: Maybe Text,
    getProfileParamsUsername :: Text
  }
  deriving (Show, Eq)

data ListArticlesParams = ListArticlesParams
  { listArticlesParamsActorId :: Maybe Text,
    listArticlesParamsTag :: Maybe Text,
    listArticlesParamsAuthor :: Maybe Text,
    listArticlesParamsFavorited :: Maybe Text,
    listArticlesParamsLimit :: Maybe Int,
    listArticlesParamsOffset :: Maybe Int
  }
  deriving (Show, Eq)

data FeedArticlesParams = FeedArticlesParams
  { feedArticlesParamsActorId :: Text,
    feedArticlesParamsLimit :: Maybe Int,
    feedArticlesParamsOffset :: Maybe Int
  }
  deriving (Show, Eq)

data GetArticleParams = GetArticleParams
  { getArticleParamsActorId :: Maybe Text,
    getArticleParamsSlug :: Text
  }
  deriving (Show, Eq)

data GetCommentsFromArticleParams = GetCommentsFromArticleParams
  { getCommentsFromArticleParamsActorId :: Maybe Text,
    getCommentsFromArticleParamsSlug :: Text
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

userToProfile :: User -> Bool -> Profile
userToProfile User {..} following =
  Profile
    { profileUsername = userUsername,
      profileBio = userBio,
      profileImage = userImage,
      profileFollowing = following
    }

data Article = Article
  { articleSlug :: Text,
    articleTitle :: Text,
    articleDescription :: Text,
    articleBody :: Text,
    articleTagList :: [Text],
    articleCreatedAt :: UTCTime,
    articleUpdatedAt :: Maybe UTCTime,
    articleFavorited :: Bool,
    articleFavoritesCount :: Int,
    articleAuthor :: Profile
  }
  deriving (Generic, Show, Eq)

data ArticleList = ArticleList
  { articleListArticles :: [Article],
    articleListArticlesCount :: Int
  }
  deriving (Generic, Show, Eq)

data Comment = Comment
  { commentId :: Text,
    commentCreatedAt :: UTCTime,
    commentUpdatedAt :: Maybe UTCTime,
    commentBody :: Text,
    commentAuthor :: Profile
  }
  deriving (Generic, Show, Eq)

data CommentList = CommentList
  { commentListComments :: [Comment]
  }
  deriving (Generic, Show, Eq)

data TagList = TagList
  { tagListTags :: [Text]
  }
  deriving (Generic, Show, Eq)