{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module RealWorld.Infra.Web.Controller.Article where

import Data.Aeson (FromJSON (parseJSON), ToJSON, genericParseJSON)
import Data.Aeson.Casing (aesonDrop, camelCase)
import Data.Aeson.Types (emptyObject)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.ArticleRepository
  ( ArticleRepository,
  )
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.Article.UseCase
  ( CreateArticleCommand (..),
    CreateArticleResult (..),
    UpdateArticleCommand (..),
    UpdateArticleResult (..),
  )
import qualified RealWorld.Domain.Command.Article.UseCase as ArticleUseCase
import RealWorld.Domain.Query.Data (Article (..), Profile)
import qualified RealWorld.Domain.Query.Data as Query
import RealWorld.Domain.Query.Service (QueryService)
import qualified RealWorld.Domain.Query.Service as QueryService
import RealWorld.Infra.Json ()
import RealWorld.Infra.Web.ErrorResponse (ErrorResponse, invalid, notFound)
import RealWorld.Infra.Web.Util (withToken, (!?))
import Relude
import Web.Scotty.Trans
  ( ActionT,
    json,
    jsonData,
    param,
    raise,
  )

data ArticleWrapper a = ArticleWrapper
  { article :: a
  }
  deriving (Show, Generic, FromJSON, ToJSON)

----------------------------------------------------------------------------------------------------
-- Create Article

data CreateArticleInput = CreateArticleInput
  { createArticleInputTitle :: Text,
    createArticleInputDescription :: Text,
    createArticleInputBody :: Text,
    createArticleInputTagList :: [Text]
  }
  deriving (Show, Generic)

instance FromJSON CreateArticleInput where
  parseJSON = genericParseJSON $ aesonDrop 18 camelCase

createArticle ::
  (MonadIO m, ArticleRepository m, UserRepository m, TxManager m, TokenGateway m, QueryService m) =>
  ActionT ErrorResponse m ()
createArticle = do
  withToken $ \token -> do
    ArticleWrapper input <- jsonData
    result <- lift $ ArticleUseCase.createArticle $ toCommand token input
    case result of
      Right result'@CreateArticleResult {..} -> do
        let params = Query.GetProfileParams Nothing createArticleResultAuthorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle input result' profile
      Left err -> raise $ invalid $ show err
  where
    toCommand :: Text -> CreateArticleInput -> ArticleUseCase.CreateArticleCommand
    toCommand token CreateArticleInput {..} =
      CreateArticleCommand
        { createArticleCommandTitle = createArticleInputTitle,
          createArticleCommandDescription = createArticleInputDescription,
          createArticleCommandBody = createArticleInputBody,
          createArticleCommandTagList = createArticleInputTagList,
          createArticleCommandToken = token
        }
    toArticle :: CreateArticleInput -> ArticleUseCase.CreateArticleResult -> Profile -> Article
    toArticle CreateArticleInput {..} CreateArticleResult {..} author =
      Article
        { articleSlug = createArticleResultSlug,
          articleTitle = createArticleInputTitle,
          articleDescription = createArticleInputDescription,
          articleBody = createArticleInputBody,
          articleTagList = createArticleInputTagList,
          articleCreatedAt = createArticleResultCreatedAt,
          articleUpdatedAt = Nothing,
          articleFavorited = False,
          articleFavoritesCount = 0,
          articleAuthor = author
        }

----------------------------------------------------------------------------------------------------
-- Update Article

data UpdateArticleInput = UpdateArticleInput
  { updateArticleInputTitle :: Maybe Text,
    updateArticleInputDescription :: Maybe Text,
    updateArticleInputBody :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON UpdateArticleInput where
  parseJSON = genericParseJSON $ aesonDrop 18 camelCase

updateArticle ::
  (MonadIO m, ArticleRepository m, UserRepository m, TxManager m, TokenGateway m, QueryService m) =>
  ActionT ErrorResponse m ()
updateArticle = do
  withToken $ \token -> do
    ArticleWrapper input <- jsonData
    slug <- param "slug"
    result <- lift $ ArticleUseCase.updateArticle $ toCommand token slug input
    case result of
      Right result'@UpdateArticleResult {..} -> do
        let params = Query.GetProfileParams Nothing updateArticleResultAuthorUsername
        profile <- QueryService.getProfile params !? notFound "Author not found"
        json $ ArticleWrapper $ toArticle result' profile
      Left err -> raise $ invalid $ show err
  where
    toCommand :: Text -> Text -> UpdateArticleInput -> ArticleUseCase.UpdateArticleCommand
    toCommand token slug UpdateArticleInput {..} =
      UpdateArticleCommand
        { updateArticleCommandToken = token,
          updateArticleCommandSlug = slug,
          updateArticleCommandTitle = updateArticleInputTitle,
          updateArticleCommandDescription = updateArticleInputDescription,
          updateArticleCommandBody = updateArticleInputBody
        }
    toArticle :: ArticleUseCase.UpdateArticleResult -> Profile -> Article
    toArticle UpdateArticleResult {..} author =
      Article
        { articleSlug = updateArticleResultSlug,
          articleTitle = updateArticleResultTitle,
          articleDescription = updateArticleResultDescription,
          articleBody = updateArticleResultBody,
          articleTagList = updateArticleResultTags,
          articleCreatedAt = updateArticleResultCreatedAt,
          articleUpdatedAt = updateArticleResultUpdatedAt,
          articleFavorited = False,
          articleFavoritesCount = updateArticleResultFavoritesCount,
          articleAuthor = author
        }

----------------------------------------------------------------------------------------------------
-- Delete Article

deleteArticle ::
  (MonadIO m, ArticleRepository m, TxManager m, TokenGateway m) =>
  ActionT ErrorResponse m ()
deleteArticle = do
  withToken $ \token -> do
    slug <- param "slug"
    result <- lift $ ArticleUseCase.deleteArticle $ toCommand token slug
    case result of
      Right _ -> json emptyObject
      Left err -> raise $ invalid $ show err
  where
    toCommand :: Text -> Text -> ArticleUseCase.DeleteArticleCommand
    toCommand = ArticleUseCase.DeleteArticleCommand
