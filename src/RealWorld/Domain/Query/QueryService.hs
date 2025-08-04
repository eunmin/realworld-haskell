{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Domain.Query.QueryService where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import RealWorld.Domain.Query.Data (
  Article,
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
import Relude

data QueryService :: Effect where
  GetCurrentUser :: GetCurrentUserParams -> QueryService m (Maybe User)
  GetProfile :: GetProfileParams -> QueryService m (Maybe Profile)
  ListArticles :: ListArticlesParams -> QueryService m ArticleList
  FeedArticles :: FeedArticlesParams -> QueryService m ArticleList
  GetArticle :: GetArticleParams -> QueryService m (Maybe Article)
  GetComments :: GetCommentsParams -> QueryService m CommentList
  GetTags :: QueryService m TagList

makeEffect ''QueryService
