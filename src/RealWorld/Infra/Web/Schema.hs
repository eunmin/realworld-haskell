module RealWorld.Infra.Web.Schema where

import Data.Swagger (ToSchema)
import RealWorld.Domain.Query.Data (Article, ArticleList, Comment, CommentList, Profile, TagList, User)

instance ToSchema User

instance ToSchema Profile

instance ToSchema Article

instance ToSchema ArticleList

instance ToSchema Comment

instance ToSchema CommentList

instance ToSchema TagList