{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module RealWorld.Infra.Web.Controller.Article where

import Data.Aeson (FromJSON (parseJSON), ToJSON, genericParseJSON)
import Data.Aeson.Casing (aesonDrop, camelCase)
import Data.Aeson.Types (emptyObject)
import Katip
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import RealWorld.Domain.Adapter.Gateway.TokenGateway qualified as TokenGateway
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository (
  ArticleRepository,
 )
import RealWorld.Domain.Adapter.Repository.CommentRepository (CommentRepository)
import RealWorld.Domain.Adapter.Repository.FavoriteRepository (FavoriteRepository)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.Article.UseCase (
  AddCommentsResult (..),
  CreateArticleCommand (..),
  CreateArticleResult (..),
  FavoriteArticleResult (..),
  UnfavoriteArticleResult (..),
  UpdateArticleCommand (..),
  UpdateArticleResult (..),
 )
import RealWorld.Domain.Command.Article.UseCase qualified as ArticleUseCase
import RealWorld.Domain.Command.User.Value (Token (..))
import RealWorld.Domain.Query.Data (Article (..), Comment (..), Profile)
import RealWorld.Domain.Query.Data qualified as Query
import RealWorld.Domain.Query.QueryService (QueryService)
import RealWorld.Domain.Query.QueryService qualified as QueryService
import RealWorld.Infra.Converter.Aeson ()
import RealWorld.Infra.Web.ErrorResponse (ErrorResponse, invalid, notFound, unauthorized)
import RealWorld.Infra.Web.Errors ()
import RealWorld.Infra.Web.Util (withOptionalToken, withRequiredToken, (!?))
import Web.Scotty.Trans (
  ActionT,
  json,
  jsonData,
  param,
  raise,
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

listArticles :: (MonadIO m, QueryService m, TokenGateway m) => ActionT ErrorResponse m ()
listArticles = do
  withOptionalToken $ \token -> do
    userId <- case Token <$> token of
      Just token' -> lift $ TokenGateway.verify token'
      Nothing -> pure Nothing
    tag <- optional $ param "tag"
    author <- optional $ param "author"
    favorited <- optional $ param "favorited"
    limit <- optional $ param "limit" <|> pure 20
    offset <- optional $ param "offset" <|> pure 0
    let params =
          Query.ListArticlesParams
            { listArticlesParamsActorId = show <$> userId
            , listArticlesParamsTag = tag
            , listArticlesParamsAuthor = author
            , listArticlesParamsFavorited = favorited
            , listArticlesParamsLimit = limit
            , listArticlesParamsOffset = offset
            }
    json =<< lift (QueryService.listArticles params)

----------------------------------------------------------------------------------------------------
-- Feed Articles

feedArticles :: (MonadIO m, QueryService m, TokenGateway m) => ActionT ErrorResponse m ()
feedArticles = do
  withRequiredToken $ \token -> do
    userId <- TokenGateway.verify (Token token) !? unauthorized "Unauthorized"
    limit <- fromMaybe 20 <$> optional (param "limit")
    offset <- fromMaybe 0 <$> optional (param "offset")
    let params =
          Query.FeedArticlesParams
            { feedArticlesParamsActorId = show userId
            , feedArticlesParamsLimit = limit
            , feedArticlesParamsOffset = offset
            }
    json =<< lift (QueryService.feedArticles params)

----------------------------------------------------------------------------------------------------
-- Get Article

getArticle :: (MonadIO m, QueryService m) => ActionT ErrorResponse m ()
getArticle = do
  slug <- param "slug"
  json =<< lift (QueryService.getArticle (Query.GetArticleParams slug))

----------------------------------------------------------------------------------------------------
-- Create Article

data CreateArticleInput = CreateArticleInput
  { createArticleInputTitle :: Text
  , createArticleInputDescription :: Text
  , createArticleInputBody :: Text
  , createArticleInputTagList :: [Text]
  }
  deriving stock (Show, Generic)

instance FromJSON CreateArticleInput where
  parseJSON = genericParseJSON $ aesonDrop 18 camelCase

createArticle ::
  ( KatipContext m
  , ArticleRepository m
  , UserRepository m
  , TxManager m
  , TokenGateway m
  , QueryService m
  ) =>
  ActionT ErrorResponse m ()
createArticle = do
  withRequiredToken $ \token -> do
    ArticleWrapper input <- jsonData
    result <- lift $ ArticleUseCase.createArticle $ toCommand token input
    case result of
      Right result'@CreateArticleResult{..} -> do
        let params = Query.GetProfileParams Nothing createArticleResultAuthorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle input result' profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "createArticle error"
        raise $ invalid $ show err
 where
  toCommand :: Text -> CreateArticleInput -> ArticleUseCase.CreateArticleCommand
  toCommand token CreateArticleInput{..} =
    CreateArticleCommand
      { createArticleCommandTitle = createArticleInputTitle
      , createArticleCommandDescription = createArticleInputDescription
      , createArticleCommandBody = createArticleInputBody
      , createArticleCommandTagList = createArticleInputTagList
      , createArticleCommandToken = token
      }
  toArticle :: CreateArticleInput -> ArticleUseCase.CreateArticleResult -> Profile -> Article
  toArticle CreateArticleInput{..} CreateArticleResult{..} author =
    Article
      { articleSlug = createArticleResultSlug
      , articleTitle = createArticleInputTitle
      , articleDescription = createArticleInputDescription
      , articleBody = createArticleInputBody
      , articleTagList = createArticleInputTagList
      , articleCreatedAt = createArticleResultCreatedAt
      , articleUpdatedAt = Nothing
      , articleFavorited = False
      , articleFavoritesCount = 0
      , articleAuthor = author
      }

----------------------------------------------------------------------------------------------------
-- Update Article

data UpdateArticleInput = UpdateArticleInput
  { updateArticleInputTitle :: Maybe Text
  , updateArticleInputDescription :: Maybe Text
  , updateArticleInputBody :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON UpdateArticleInput where
  parseJSON = genericParseJSON $ aesonDrop 18 camelCase

updateArticle ::
  ( KatipContext m
  , ArticleRepository m
  , UserRepository m
  , FavoriteRepository m
  , TxManager m
  , TokenGateway m
  , QueryService m
  ) =>
  ActionT ErrorResponse m ()
updateArticle = do
  withRequiredToken $ \token -> do
    ArticleWrapper input <- jsonData
    slug <- param "slug"
    result <- lift $ ArticleUseCase.updateArticle $ toCommand token slug input
    case result of
      Right result'@UpdateArticleResult{..} -> do
        let params = Query.GetProfileParams Nothing updateArticleResultAuthorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle result' profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "updateArticle error"
        raise $ invalid $ show err
 where
  toCommand :: Text -> Text -> UpdateArticleInput -> ArticleUseCase.UpdateArticleCommand
  toCommand token slug UpdateArticleInput{..} =
    UpdateArticleCommand
      { updateArticleCommandToken = token
      , updateArticleCommandSlug = slug
      , updateArticleCommandTitle = updateArticleInputTitle
      , updateArticleCommandDescription = updateArticleInputDescription
      , updateArticleCommandBody = updateArticleInputBody
      }
  toArticle :: ArticleUseCase.UpdateArticleResult -> Profile -> Article
  toArticle UpdateArticleResult{..} author =
    Article
      { articleSlug = updateArticleResultSlug
      , articleTitle = updateArticleResultTitle
      , articleDescription = updateArticleResultDescription
      , articleBody = updateArticleResultBody
      , articleTagList = updateArticleResultTags
      , articleCreatedAt = updateArticleResultCreatedAt
      , articleUpdatedAt = updateArticleResultUpdatedAt
      , articleFavorited = updateArticleResultFavorited
      , articleFavoritesCount = updateArticleResultFavoritesCount
      , articleAuthor = author
      }

----------------------------------------------------------------------------------------------------
-- Delete Article

deleteArticle ::
  (KatipContext m, ArticleRepository m, TxManager m, TokenGateway m) =>
  ActionT ErrorResponse m ()
deleteArticle = do
  withRequiredToken $ \token -> do
    slug <- param "slug"
    result <- lift $ ArticleUseCase.deleteArticle $ toCommand token slug
    case result of
      Right _ -> json emptyObject
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "deleteArticle error"
        raise $ invalid $ show err
 where
  toCommand :: Text -> Text -> ArticleUseCase.DeleteArticleCommand
  toCommand = ArticleUseCase.DeleteArticleCommand

----------------------------------------------------------------------------------------------------
-- Add Comments to an Article

data AddCommentsInput = AddCommentsInput
  { addCommentsInputBody :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON AddCommentsInput where
  parseJSON = genericParseJSON $ aesonDrop 16 camelCase

addComments ::
  ( KatipContext m
  , ArticleRepository m
  , TxManager m
  , TokenGateway m
  , UserRepository m
  , CommentRepository m
  , QueryService m
  ) =>
  ActionT ErrorResponse m ()
addComments = do
  withRequiredToken $ \token -> do
    slug <- param "slug"
    CommentWrapper input <- jsonData
    result <- lift $ ArticleUseCase.addComments $ toCommand token slug input
    case result of
      Right result'@AddCommentsResult{..} -> do
        let params = Query.GetProfileParams Nothing addCommentsResultAuthorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ CommentWrapper $ toComment result' (addCommentsInputBody input) profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "addComments error"
        raise $ invalid $ show err
 where
  toCommand :: Text -> Text -> AddCommentsInput -> ArticleUseCase.AddCommentsCommand
  toCommand token slug AddCommentsInput{..} =
    ArticleUseCase.AddCommentsCommand
      { addCommentsCommandToken = token
      , addCommentsCommandSlug = slug
      , addCommentsCommandBody = addCommentsInputBody
      }
  toComment :: ArticleUseCase.AddCommentsResult -> Text -> Profile -> Query.Comment
  toComment AddCommentsResult{..} body profile =
    Comment
      { commentId = addCommentsResultCommentId
      , commentCreatedAt = addCommentsResultCreatedAt
      , commentUpdatedAt = Nothing
      , commentBody = body
      , commentAuthor = profile
      }

----------------------------------------------------------------------------------------------------
-- Get Comments from an Article

getComments :: (MonadIO m, QueryService m, TokenGateway m) => ActionT ErrorResponse m ()
getComments = do
  withOptionalToken $ \token -> do
    userId <- case Token <$> token of
      Just token' -> lift $ TokenGateway.verify token'
      Nothing -> pure Nothing
    slug <- param "slug"
    let params =
          Query.GetCommentsParams
            { getCommentsParamsActorId = show <$> userId
            , getCommentsParamsSlug = slug
            }
    json =<< lift (QueryService.getComments params)

----------------------------------------------------------------------------------------------------
-- Delete Comment

deleteComment ::
  (KatipContext m, ArticleRepository m, CommentRepository m, TxManager m, TokenGateway m) =>
  ActionT ErrorResponse m ()
deleteComment = do
  withRequiredToken $ \token -> do
    slug <- param "slug"
    commentId <- param "comment-id"
    result <- lift $ ArticleUseCase.deleteComment $ toCommand token slug commentId
    case result of
      Right _ -> json emptyObject
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "deleteComment error"
        raise $ invalid $ show err
 where
  toCommand :: Text -> Text -> Text -> ArticleUseCase.DeleteCommentCommand
  toCommand = ArticleUseCase.DeleteCommentCommand

----------------------------------------------------------------------------------------------------
-- Favorite Article

favorite ::
  ( KatipContext m
  , ArticleRepository m
  , FavoriteRepository m
  , UserRepository m
  , TxManager m
  , QueryService m
  , TokenGateway m
  ) =>
  ActionT ErrorResponse m ()
favorite = do
  withRequiredToken $ \token -> do
    slug <- param "slug"
    result <- lift $ ArticleUseCase.favoriteArticle $ toCommand token slug
    case result of
      Right result'@FavoriteArticleResult{..} -> do
        let params = Query.GetProfileParams Nothing favoriteArticleResultAuthorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle result' profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "favorite error"
        raise $ invalid $ show err
 where
  toCommand :: Text -> Text -> ArticleUseCase.FavoriteArticleCommand
  toCommand = ArticleUseCase.FavoriteArticleCommand
  toArticle :: ArticleUseCase.FavoriteArticleResult -> Profile -> Article
  toArticle FavoriteArticleResult{..} author =
    Article
      { articleSlug = favoriteArticleResultSlug
      , articleTitle = favoriteArticleResultTitle
      , articleDescription = favoriteArticleResultDescription
      , articleBody = favoriteArticleResultBody
      , articleTagList = favoriteArticleResultTags
      , articleCreatedAt = favoriteArticleResultCreatedAt
      , articleUpdatedAt = favoriteArticleResultUpdatedAt
      , articleFavorited = True
      , articleFavoritesCount = favoriteArticleResultFavoritesCount
      , articleAuthor = author
      }

----------------------------------------------------------------------------------------------------
-- Unfavorite Article

unfavorite ::
  ( KatipContext m
  , ArticleRepository m
  , FavoriteRepository m
  , UserRepository m
  , TxManager m
  , QueryService m
  , TokenGateway m
  ) =>
  ActionT ErrorResponse m ()
unfavorite = do
  withRequiredToken $ \token -> do
    slug <- param "slug"
    result <- lift $ ArticleUseCase.unfavoriteArticle $ toCommand token slug
    case result of
      Right result'@UnfavoriteArticleResult{..} -> do
        let params = Query.GetProfileParams Nothing unfavoriteArticleResultAuthorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle result' profile
      Left err -> do
        lift $ katipAddContext (sl "error" err) $ do
          $(logTM) ErrorS "unfavorite error"
        raise $ invalid $ show err
 where
  toCommand :: Text -> Text -> ArticleUseCase.UnfavoriteArticleCommand
  toCommand = ArticleUseCase.UnfavoriteArticleCommand
  toArticle :: ArticleUseCase.UnfavoriteArticleResult -> Profile -> Article
  toArticle UnfavoriteArticleResult{..} author =
    Article
      { articleSlug = unfavoriteArticleResultSlug
      , articleTitle = unfavoriteArticleResultTitle
      , articleDescription = unfavoriteArticleResultDescription
      , articleBody = unfavoriteArticleResultBody
      , articleTagList = unfavoriteArticleResultTags
      , articleCreatedAt = unfavoriteArticleResultCreatedAt
      , articleUpdatedAt = unfavoriteArticleResultUpdatedAt
      , articleFavorited = False
      , articleFavoritesCount = unfavoriteArticleResultFavoritesCount
      , articleAuthor = author
      }

----------------------------------------------------------------------------------------------------
-- Get Tags

getTags :: (MonadIO m, QueryService m) => ActionT ErrorResponse m ()
getTags = json =<< lift QueryService.getTags