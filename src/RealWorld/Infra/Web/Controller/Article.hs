{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module RealWorld.Infra.Web.Controller.Article where

import Data.Aeson (FromJSON (parseJSON), ToJSON, genericParseJSON)
import Data.Aeson.Casing (aesonDrop, camelCase)
import Data.Aeson.Types (emptyObject)
import Katip
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import qualified RealWorld.Domain.Adapter.Gateway.TokenGateway as TokenGateway
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository
  ( ArticleRepository,
  )
import RealWorld.Domain.Adapter.Repository.CommentRepository (CommentRepository)
import RealWorld.Domain.Adapter.Repository.FavoriteRepository (FavoriteRepository)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.Article.UseCase
  ( AddCommentsResult (..),
    CreateArticleCommand (..),
    CreateArticleResult (..),
    FavoriteArticleResult (..),
    UnfavoriteArticleResult (..),
    UpdateArticleCommand (..),
    UpdateArticleResult (..),
  )
import qualified RealWorld.Domain.Command.Article.UseCase as ArticleUseCase
import RealWorld.Domain.Command.User.Value (Token (..))
import RealWorld.Domain.Query.Data (Article (..), Comment (..), Profile)
import qualified RealWorld.Domain.Query.Data as Query
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Converter.Aeson ()
import RealWorld.Infra.Web.ErrorResponse (invalid, notFound, unauthorized)
import RealWorld.Infra.Web.Errors ()
import RealWorld.Infra.Web.Util (withOptionalToken, withRequiredToken, (!?))
import Relude
import Web.Scotty.Trans
  ( ActionT,
    json,
    jsonData,
    pathParam,
    queryParamMaybe,
    throw,
  )

data ArticleWrapper a = ArticleWrapper
  { article :: a
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CommentWrapper a = CommentWrapper
  { comment :: a
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

----------------------------------------------------------------------------------------------------
-- List Articles

listArticles :: (MonadIO m, QueryService m, TokenGateway m) => ActionT m ()
listArticles = do
  withOptionalToken $ \token -> do
    userId <- case Token <$> token of
      Just token' -> lift $ TokenGateway.verify token'
      Nothing -> pure Nothing
    tag <- queryParamMaybe "tag"
    author <- queryParamMaybe "author"
    favorited <- queryParamMaybe "favorited"
    limit <- queryParamMaybe "limit"
    offset <- queryParamMaybe "offset"
    let params =
          Query.ListArticlesParams
            { actorId = show <$> userId,
              tag = tag,
              author = author,
              favorited = favorited,
              limit = Just $ fromMaybe 20 limit,
              offset = Just $ fromMaybe 0 offset
            }
    json =<< lift (QueryService.listArticles params)

----------------------------------------------------------------------------------------------------
-- Feed Articles

feedArticles :: (MonadIO m, QueryService m, TokenGateway m) => ActionT m ()
feedArticles = do
  withRequiredToken $ \token -> do
    userId <- TokenGateway.verify (Token token) !? unauthorized "Unauthorized"
    limit <- queryParamMaybe "limit"
    offset <- queryParamMaybe "offset"
    let params =
          Query.FeedArticlesParams
            { actorId = show userId,
              limit = fromMaybe 20 limit,
              offset = fromMaybe 0 offset
            }
    json =<< lift (QueryService.feedArticles params)

----------------------------------------------------------------------------------------------------
-- Get Article

getArticle :: (MonadIO m, QueryService m) => ActionT m ()
getArticle = do
  slug <- pathParam "slug"
  json =<< lift (QueryService.getArticle (Query.GetArticleParams slug))

----------------------------------------------------------------------------------------------------
-- Create Article

data CreateArticleInput = CreateArticleInput
  { title :: Text,
    description :: Text,
    body :: Text,
    tagList :: [Text]
  }
  deriving stock (Show, Generic)

instance FromJSON CreateArticleInput where
  parseJSON = genericParseJSON $ aesonDrop 18 camelCase

createArticle ::
  ( KatipContext m,
    ArticleRepository m,
    UserRepository m,
    TxManager m,
    TokenGateway m,
    QueryService m
  ) =>
  ActionT m ()
createArticle = do
  withRequiredToken $ \token -> do
    ArticleWrapper input <- jsonData
    result <- lift $ ArticleUseCase.createArticle $ toCommand token input
    case result of
      Right result' -> do
        let params = Query.GetProfileParams Nothing result'.authorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle input result' profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "createArticle error"
        throw $ invalid $ show err
  where
    toCommand :: Text -> CreateArticleInput -> ArticleUseCase.CreateArticleCommand
    toCommand token input =
      CreateArticleCommand
        { title = input.title,
          description = input.description,
          body = input.body,
          tagList = input.tagList,
          token = token
        }
    toArticle :: CreateArticleInput -> ArticleUseCase.CreateArticleResult -> Profile -> Article
    toArticle input result author =
      Article
        { slug = result.slug,
          title = input.title,
          description = input.description,
          body = input.body,
          tagList = input.tagList,
          createdAt = result.createdAt,
          updatedAt = Nothing,
          favorited = False,
          favoritesCount = 0,
          author = author
        }

----------------------------------------------------------------------------------------------------
-- Update Article

data UpdateArticleInput = UpdateArticleInput
  { title :: Maybe Text,
    description :: Maybe Text,
    body :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON UpdateArticleInput where
  parseJSON = genericParseJSON $ aesonDrop 18 camelCase

updateArticle ::
  ( KatipContext m,
    ArticleRepository m,
    UserRepository m,
    FavoriteRepository m,
    TxManager m,
    TokenGateway m,
    QueryService m
  ) =>
  ActionT m ()
updateArticle = do
  withRequiredToken $ \token -> do
    ArticleWrapper input <- jsonData
    slug <- pathParam "slug"
    result <- lift $ ArticleUseCase.updateArticle $ toCommand token slug input
    case result of
      Right result' -> do
        let params = Query.GetProfileParams Nothing result'.authorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle result' profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "updateArticle error"
        throw $ invalid $ show err
  where
    toCommand :: Text -> Text -> UpdateArticleInput -> ArticleUseCase.UpdateArticleCommand
    toCommand token slug input =
      UpdateArticleCommand
        { token = token,
          slug = slug,
          title = input.title,
          description = input.description,
          body = input.body
        }
    toArticle :: ArticleUseCase.UpdateArticleResult -> Profile -> Article
    toArticle result author =
      Article
        { slug = result.slug,
          title = result.title,
          description = result.description,
          body = result.body,
          tagList = result.tags,
          createdAt = result.createdAt,
          updatedAt = result.updatedAt,
          favorited = result.favorited,
          favoritesCount = result.favoritesCount,
          author = author
        }

----------------------------------------------------------------------------------------------------
-- Delete Article

deleteArticle ::
  (KatipContext m, ArticleRepository m, TxManager m, TokenGateway m) =>
  ActionT m ()
deleteArticle = do
  withRequiredToken $ \token -> do
    slug <- pathParam "slug"
    result <- lift $ ArticleUseCase.deleteArticle $ toCommand token slug
    case result of
      Right _ -> json emptyObject
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "deleteArticle error"
        throw $ invalid $ show err
  where
    toCommand :: Text -> Text -> ArticleUseCase.DeleteArticleCommand
    toCommand = ArticleUseCase.DeleteArticleCommand

----------------------------------------------------------------------------------------------------
-- Add Comments to an Article

data AddCommentsInput = AddCommentsInput
  { body :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON AddCommentsInput where
  parseJSON = genericParseJSON $ aesonDrop 16 camelCase

addComments ::
  ( KatipContext m,
    ArticleRepository m,
    TxManager m,
    TokenGateway m,
    UserRepository m,
    CommentRepository m,
    QueryService m
  ) =>
  ActionT m ()
addComments = do
  withRequiredToken $ \token -> do
    slug <- pathParam "slug"
    CommentWrapper input <- jsonData
    result <- lift $ ArticleUseCase.addComments $ toCommand token slug input
    case result of
      Right result' -> do
        let params = Query.GetProfileParams Nothing result'.authorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ CommentWrapper $ toComment result' input.body profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "addComments error"
        throw $ invalid $ show err
  where
    toCommand :: Text -> Text -> AddCommentsInput -> ArticleUseCase.AddCommentsCommand
    toCommand token slug input =
      ArticleUseCase.AddCommentsCommand
        { token = token,
          slug = slug,
          body = input.body
        }
    toComment :: ArticleUseCase.AddCommentsResult -> Text -> Profile -> Query.Comment
    toComment result body profile =
      Comment
        { commentId = result.commentId,
          createdAt = result.createdAt,
          updatedAt = Nothing,
          body = body,
          author = profile
        }

----------------------------------------------------------------------------------------------------
-- Get Comments from an Article

getComments :: (MonadIO m, QueryService m, TokenGateway m) => ActionT m ()
getComments = do
  withOptionalToken $ \token -> do
    userId <- case Token <$> token of
      Just token' -> lift $ TokenGateway.verify token'
      Nothing -> pure Nothing
    slug <- pathParam "slug"
    let params =
          Query.GetCommentsParams
            { actorId = show <$> userId,
              slug = slug
            }
    json =<< lift (QueryService.getComments params)

----------------------------------------------------------------------------------------------------
-- Delete Comment

deleteComment ::
  (KatipContext m, ArticleRepository m, CommentRepository m, TxManager m, TokenGateway m) =>
  ActionT m ()
deleteComment = do
  withRequiredToken $ \token -> do
    slug <- pathParam "slug"
    commentId <- pathParam "comment-id"
    result <- lift $ ArticleUseCase.deleteComment $ toCommand token slug commentId
    case result of
      Right _ -> json emptyObject
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "deleteComment error"
        throw $ invalid $ show err
  where
    toCommand :: Text -> Text -> Text -> ArticleUseCase.DeleteCommentCommand
    toCommand = ArticleUseCase.DeleteCommentCommand

----------------------------------------------------------------------------------------------------
-- Favorite Article

favorite ::
  ( KatipContext m,
    ArticleRepository m,
    FavoriteRepository m,
    UserRepository m,
    TxManager m,
    QueryService m,
    TokenGateway m
  ) =>
  ActionT m ()
favorite = do
  withRequiredToken $ \token -> do
    slug <- pathParam "slug"
    result <- lift $ ArticleUseCase.favoriteArticle $ toCommand token slug
    case result of
      Right result' -> do
        let params = Query.GetProfileParams Nothing result'.authorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle result' profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "favorite error"
        throw $ invalid $ show err
  where
    toCommand :: Text -> Text -> ArticleUseCase.FavoriteArticleCommand
    toCommand = ArticleUseCase.FavoriteArticleCommand
    toArticle :: ArticleUseCase.FavoriteArticleResult -> Profile -> Article
    toArticle result author =
      Article
        { slug = result.slug,
          title = result.title,
          description = result.description,
          body = result.body,
          tagList = result.tags,
          createdAt = result.createdAt,
          updatedAt = result.updatedAt,
          favorited = True,
          favoritesCount = result.favoritesCount,
          author = author
        }

----------------------------------------------------------------------------------------------------
-- Unfavorite Article

unfavorite ::
  ( KatipContext m,
    ArticleRepository m,
    FavoriteRepository m,
    UserRepository m,
    TxManager m,
    QueryService m,
    TokenGateway m
  ) =>
  ActionT m ()
unfavorite = do
  withRequiredToken $ \token -> do
    slug <- pathParam "slug"
    result <- lift $ ArticleUseCase.unfavoriteArticle $ toCommand token slug
    case result of
      Right result' -> do
        let params = Query.GetProfileParams Nothing result'.authorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle result' profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "unfavorite error"
        throw $ invalid $ show err
  where
    toCommand :: Text -> Text -> ArticleUseCase.UnfavoriteArticleCommand
    toCommand = ArticleUseCase.UnfavoriteArticleCommand
    toArticle :: ArticleUseCase.UnfavoriteArticleResult -> Profile -> Article
    toArticle result author =
      Article
        { slug = result.slug,
          title = result.title,
          description = result.description,
          body = result.body,
          tagList = result.tags,
          createdAt = result.createdAt,
          updatedAt = result.updatedAt,
          favorited = False,
          favoritesCount = result.favoritesCount,
          author = author
        }

----------------------------------------------------------------------------------------------------
-- Get Tags

getTags :: (MonadIO m, QueryService m) => ActionT m ()
getTags = json =<< lift QueryService.getTags
