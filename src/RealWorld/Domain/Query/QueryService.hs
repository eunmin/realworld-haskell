module RealWorld.Domain.Query.QueryService where

import RealWorld.Domain.Query.Data
  ( Article,
    ArticleList,
    CommentList,
    FeedArticlesParams,
    GetArticleParams,
    GetCommentsParams,
    GetCurrentUserParams,
    GetProfileParams,
    ListArticlesParams,
    Profile,
    TagList,
    User,
  )

class QueryService m where
  getCurrentUser :: GetCurrentUserParams -> m (Maybe User)
  getProfile :: GetProfileParams -> m (Maybe Profile)
  listArticles :: ListArticlesParams -> m ArticleList
  feedArticles :: FeedArticlesParams -> m ArticleList
  getArticle :: GetArticleParams -> m (Maybe Article)
  getComments :: GetCommentsParams -> m CommentList
  getTags :: m TagList