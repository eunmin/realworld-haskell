{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Routes where

import Control.Lens (Traversal', (%~), (.~), (?~))
import Control.Monad.Except (MonadError)
import qualified Data.HashSet.InsOrd as InsOrdHS
import Data.Swagger (
  ApiKeyLocation (ApiKeyHeader),
  ApiKeyParams (ApiKeyParams),
  HasDescription (description),
  HasInfo (info),
  HasSecurity (security),
  HasSecurityDefinitions (securityDefinitions),
  HasTags (tags),
  HasTitle (title),
  HasVersion (version),
  Operation,
  SecurityDefinitions (..),
  SecurityRequirement (SecurityRequirement),
  SecurityScheme (
    SecurityScheme,
    _securitySchemeDescription,
    _securitySchemeType
  ),
  SecuritySchemeType (SecuritySchemeApiKey),
  Swagger,
 )
import Katip (KatipContext)
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository)
import RealWorld.Domain.Adapter.Repository.CommentRepository (CommentRepository)
import RealWorld.Domain.Adapter.Repository.FavoriteRepository (FavoriteRepository)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Query.QueryService (QueryService)
import RealWorld.Infra.Web.Auth (ApiAuth, AuthContext)
import qualified RealWorld.Infra.Web.Handler.Article.CreateArticle as Article.CreateArticle
import qualified RealWorld.Infra.Web.Handler.Article.CreateArticle as Article.GetArticle
import qualified RealWorld.Infra.Web.Handler.Article.CreateComment as Article.CreateComment
import qualified RealWorld.Infra.Web.Handler.Article.DeleteArticle as Article.DeleteArticle
import qualified RealWorld.Infra.Web.Handler.Article.DeleteComment as Article.DeleteComment
import qualified RealWorld.Infra.Web.Handler.Article.Favorite as Article.Favorite
import qualified RealWorld.Infra.Web.Handler.Article.Feed as Article.Feed
import qualified RealWorld.Infra.Web.Handler.Article.ListArticle as Article.ListArticle
import qualified RealWorld.Infra.Web.Handler.Article.ListComment as Article.ListComment
import qualified RealWorld.Infra.Web.Handler.Article.ListTag as Article.ListTag
import qualified RealWorld.Infra.Web.Handler.Article.Unfavorite as Article.Unfavorite
import qualified RealWorld.Infra.Web.Handler.Article.UpdateArticle as Article.UpdateArticle
import qualified RealWorld.Infra.Web.Handler.Profile.Follow as Profile.Follow
import qualified RealWorld.Infra.Web.Handler.Profile.GetProfile as Profile.GetProfile
import qualified RealWorld.Infra.Web.Handler.Profile.Unfollow as Profile.Unfollow
import qualified RealWorld.Infra.Web.Handler.User.GetCurrentUser as User.GetCurrentUser
import qualified RealWorld.Infra.Web.Handler.User.Login as User.Login
import qualified RealWorld.Infra.Web.Handler.User.Registration as User.Registration
import qualified RealWorld.Infra.Web.Handler.User.UpdateUser as User.UpdateUser
import RealWorld.Infra.Web.Schema ()
import Relude hiding (get, put)
import Servant (
  AuthProtect,
  EmptyAPI,
  Handler,
  HasServer (ServerT),
  Server,
  ServerError,
  emptyServer,
  hoistServerWithContext,
  type (:<|>) (..),
  type (:>),
 )
import Servant.Swagger (HasSwagger (toSwagger), subOperations)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServerT)

type Deps m =
  ( KatipContext m
  , TxManager m
  , ArticleRepository m
  , UserRepository m
  , CommentRepository m
  , FavoriteRepository m
  , TokenGateway m
  , PasswordGateway m
  , QueryService m
  , MonadError ServerError m
  )

rootServer :: (Deps m) => (forall a. m a -> Handler a) -> Server Root
rootServer runner =
  hoistServerWithContext
    (Proxy :: Proxy Root)
    (Proxy :: Proxy AuthContext)
    runner
    rootServerT

type Root = SwaggerAPI :<|> API

rootServerT :: (Deps m) => ServerT Root m
rootServerT = swaggerServerT :<|> apiServerT

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerServerT :: (Deps m) => ServerT SwaggerAPI m
swaggerServerT = swaggerSchemaUIServerT swagger
 where
  swagger =
    toSwagger (Proxy :: Proxy AllAPI)
      & info
      . title
      .~ "RealWorld API"
        & info
        . version
      .~ "1.0"
        & info
        . description
      ?~ "https://github.com/gothinkster/realworld"
        & securityDefinitions
      .~ apiAuth
        & protectedOps
        . security
      .~ [SecurityRequirement [("apiAuth", [])]]
        & userOps
        . tags
      %~ (<> InsOrdHS.fromList ["User"])
        & articleOps
        . tags
      %~ (<> InsOrdHS.fromList ["Article"])
        & profileOps
        . tags
      %~ (<> InsOrdHS.fromList ["Profile"])

type AllAPI = UnprotectedRoute :<|> ProtectedRoute

protectedOps :: Traversal' Swagger Operation
protectedOps = subOperations (Proxy :: Proxy ProtectedRoute) (Proxy :: Proxy AllAPI)

userOps :: Traversal' Swagger Operation
userOps = subOperations (Proxy :: Proxy ("api" :> UserAPI)) (Proxy :: Proxy AllAPI)

articleOps :: Traversal' Swagger Operation
articleOps = subOperations (Proxy :: Proxy ("api" :> ArticleAPI)) (Proxy :: Proxy AllAPI)

profileOps :: Traversal' Swagger Operation
profileOps = subOperations (Proxy :: Proxy ("api" :> ProfileAPI)) (Proxy :: Proxy AllAPI)

apiAuth :: SecurityDefinitions
apiAuth =
  SecurityDefinitions
    [
      ( "apiAuth"
      , SecurityScheme
          { _securitySchemeType =
              SecuritySchemeApiKey
                (ApiKeyParams "Authorization" ApiKeyHeader)
          , _securitySchemeDescription = Just "bearer token"
          }
      )
    ]

type API =
  EmptyAPI
    :<|> UnprotectedRoute
    :<|> AuthProtect "apiAuth" :> ProtectedRoute

apiServerT :: (Deps m) => ServerT API m
apiServerT =
  emptyServer
    :<|> unprotectedServerT
    :<|> protectedServerT

type UnprotectedRoute =
  EmptyAPI
    :<|> "api" :> User.Login.Route
    :<|> "api" :> User.Registration.Route

unprotectedServerT :: (Deps m) => ServerT UnprotectedRoute m
unprotectedServerT =
  emptyServer
    :<|> User.Login.handler
    :<|> User.Registration.handler

type ProtectedRoute =
  EmptyAPI
    :<|> "api" :> User.GetCurrentUser.Route
    :<|> "api" :> User.UpdateUser.Route
    :<|> "api" :> ProfileAPI
    :<|> "api" :> ArticleAPI

protectedServerT :: (Deps m) => ApiAuth -> ServerT ProtectedRoute m
protectedServerT auth =
  emptyServer
    :<|> User.GetCurrentUser.handler auth
    :<|> User.UpdateUser.handler auth
    :<|> profileServerT auth
    :<|> articleServerT auth

type UserAPI =
  EmptyAPI
    :<|> User.Login.Route
    :<|> User.Registration.Route
    :<|> User.GetCurrentUser.Route
    :<|> User.UpdateUser.Route

type ProfileAPI =
  EmptyAPI
    :<|> Profile.GetProfile.Route
    :<|> Profile.Follow.Route
    :<|> Profile.Unfollow.Route

profileServerT :: (Deps m) => ApiAuth -> ServerT ProfileAPI m
profileServerT auth =
  emptyServer
    :<|> Profile.GetProfile.handler auth
    :<|> Profile.Follow.handler auth
    :<|> Profile.Unfollow.handler auth

type ArticleAPI =
  EmptyAPI
    :<|> Article.ListArticle.Route
    :<|> Article.Feed.Route
    :<|> Article.GetArticle.Route
    :<|> Article.CreateArticle.Route
    :<|> Article.UpdateArticle.Route
    :<|> Article.DeleteArticle.Route
    :<|> Article.CreateComment.Route
    :<|> Article.ListComment.Route
    :<|> Article.DeleteComment.Route
    :<|> Article.Favorite.Route
    :<|> Article.Unfavorite.Route
    :<|> Article.ListTag.Route

articleServerT :: (Deps m) => ApiAuth -> ServerT ArticleAPI m
articleServerT auth =
  emptyServer
    :<|> Article.ListArticle.handler auth
    :<|> Article.Feed.handler auth
    :<|> Article.GetArticle.handler auth
    :<|> Article.CreateArticle.handler auth
    :<|> Article.UpdateArticle.handler auth
    :<|> Article.DeleteArticle.handler auth
    :<|> Article.CreateComment.handler auth
    :<|> Article.ListComment.handler auth
    :<|> Article.DeleteComment.handler auth
    :<|> Article.Favorite.handler auth
    :<|> Article.Unfavorite.handler auth
    :<|> Article.ListTag.handler auth