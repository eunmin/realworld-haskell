{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module RealWorld.Domain.Command.Article.UseCase where

import Data.Aeson (ToJSON)
import Data.Time (UTCTime, getCurrentTime)
import Data.ULID (getULID)
import Effectful (Eff, IOE, type (:>))
import Effectful.Error.Dynamic (runErrorNoCallStack, throwError)
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager, withTx)
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository (..))
import qualified RealWorld.Domain.Adapter.Repository.ArticleRepository as ArticleRepository
import RealWorld.Domain.Adapter.Repository.CommentRepository (CommentRepository)
import qualified RealWorld.Domain.Adapter.Repository.CommentRepository as CommentRepository
import RealWorld.Domain.Adapter.Repository.FavoriteRepository (FavoriteRepository)
import qualified RealWorld.Domain.Adapter.Repository.FavoriteRepository as FavoriteRepository
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import qualified RealWorld.Domain.Adapter.Repository.UserRepository as UserRepository
import RealWorld.Domain.Command.Article.Entity.Article (
  Article (..),
  mkArticle,
 )
import qualified RealWorld.Domain.Command.Article.Entity.Article as Article
import RealWorld.Domain.Command.Article.Entity.Comment (mkComment)
import qualified RealWorld.Domain.Command.Article.Entity.Comment as Comment
import RealWorld.Domain.Command.Article.Entity.Favorite (mkFavorite)
import RealWorld.Domain.Command.Article.Value (
  ArticleBody (unArticleBody),
  Description (unDescription),
  FavoriteId (..),
  Slug (unSlug),
  Tag (unTag),
  Title (unTitle),
  mkArticleBody,
  mkCommentBody,
  mkDescription,
  mkFavoriteId,
  mkSlug,
  mkTag,
  mkTitle,
 )
import RealWorld.Domain.Command.User.Entity.User (User (..))
import RealWorld.Domain.Command.User.Value (Username (..))
import RealWorld.Domain.Util.BoundedText (BoundedText (..))
import Relude

----------------------------------------------------------------------------------------------------
-- Create Article

data CreateArticleCommand = CreateArticleCommand
  { title :: Text
  , description :: Text
  , body :: Text
  , tagList :: [Text]
  , userId :: Text
  }
  deriving stock (Show, Eq, Generic)

data CreateArticleResult = CreateArticleResult
  { slug :: Text
  , createdAt :: UTCTime
  , authorUsername :: Text
  }
  deriving stock (Show, Eq)

data CreateArticleError
  = CreateArticleErrorInvalidTitle
  | CreateArticleErrorInvalidUserId
  | CreateArticleErrorInvalidBody
  | CreateArticleErrorInvalidDescription
  | CreateArticleErrorInvalidTag
  | CreateArticleErrorAuthorNotFound
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

createArticle ::
  (IOE :> es, ArticleRepository :> es, UserRepository :> es, TxManager :> es) =>
  CreateArticleCommand ->
  Eff es (Either CreateArticleError CreateArticleResult)
createArticle command = runErrorNoCallStack $ do
  articleId <- liftIO getULID
  createdAt <- liftIO getCurrentTime
  authorId <- readMaybe (toString command.userId) `whenNothing` throwError CreateArticleErrorInvalidUserId
  title <- mkTitle command.title `whenNothing` throwError CreateArticleErrorInvalidTitle
  body <- mkArticleBody command.body `whenNothing` throwError CreateArticleErrorInvalidBody
  description <- mkDescription command.description `whenNothing` throwError CreateArticleErrorInvalidDescription
  tags <- traverse mkTag command.tagList `whenNothing` throwError CreateArticleErrorInvalidTag
  author <- UserRepository.findById authorId `whenNothingM` throwError CreateArticleErrorAuthorNotFound
  let article =
        mkArticle
          articleId
          title
          description
          body
          tags
          createdAt
          authorId
  _ <- withTx $ ArticleRepository.save article
  pure $
    CreateArticleResult
      { slug = unSlug article.slug
      , createdAt = createdAt
      , authorUsername = author.username.unUsername.unBoundedText
      }

----------------------------------------------------------------------------------------------------
-- Update Article

data UpdateArticleCommand = UpdateArticleCommand
  { userId :: Text
  , slug :: Text
  , title :: Maybe Text
  , description :: Maybe Text
  , body :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

data UpdateArticleResult = UpdateArticleResult
  { slug :: Text
  , title :: Text
  , description :: Text
  , body :: Text
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  , tags :: [Text]
  , favoritesCount :: Int
  , authorUsername :: Text
  , favorited :: Bool
  }
  deriving stock (Show, Eq)

data UpdateArticleError
  = UpdateArticleErrorInvalidTitle
  | UpdateArticleErrorUserId
  | UpdateArticleErrorInvalidSlug
  | UpdateArticleErrorArticleNotFound
  | UpdateArticleErrorInvalidBody
  | UpdateArticleErrorInvalidDescription
  | UpdateArticleErrorAuthorNotFound
  | UpdateArticleErrorEditPermissionDenied
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

updateArticle ::
  (ArticleRepository :> es, UserRepository :> es, FavoriteRepository :> es, TxManager :> es) =>
  UpdateArticleCommand ->
  Eff es (Either UpdateArticleError UpdateArticleResult)
updateArticle command = runErrorNoCallStack $ do
  slug <- mkSlug command.slug `whenNothing` throwError UpdateArticleErrorInvalidSlug
  title <- traverse mkTitle command.title `whenNothing` throwError UpdateArticleErrorInvalidTitle
  description <- traverse mkDescription command.description `whenNothing` throwError UpdateArticleErrorInvalidDescription
  body <- traverse mkArticleBody command.body `whenNothing` throwError UpdateArticleErrorInvalidBody
  actorId <- readMaybe (toString command.userId) `whenNothing` throwError UpdateArticleErrorUserId
  (article, author, favorited) <- withTx $ do
    article <- ArticleRepository.findBySlug slug `whenNothingM` throwError UpdateArticleErrorArticleNotFound
    author <- UserRepository.findById article.authorId `whenNothingM` throwError UpdateArticleErrorAuthorNotFound
    unless (Article.isEditable article actorId) $
      throwError UpdateArticleErrorEditPermissionDenied
    let article' = Article.update article title description body
    _ <- ArticleRepository.save article'
    favorited <- FavoriteRepository.findById (FavoriteId article.articleId actorId)
    pure (article', author, isJust favorited)
  pure $
    UpdateArticleResult
      { slug = unSlug article.slug
      , title = unTitle article.title
      , description = unDescription article.description
      , body = unArticleBody article.body
      , createdAt = article.createdAt
      , updatedAt = article.updatedAt
      , tags = unTag <$> article.tags
      , favoritesCount = article.favoritesCount
      , authorUsername = author.username.unUsername.unBoundedText
      , favorited = favorited
      }

----------------------------------------------------------------------------------------------------
-- Delete Article

data DeleteArticleCommand = DeleteArticleCommand
  { userId :: Text
  , slug :: Text
  }
  deriving stock (Show, Eq, Generic)

data DeleteArticleResult = DeleteArticleResult
  { slug :: Text
  }
  deriving stock (Show, Eq)

data DeleteArticleError
  = DeleteArticleErrorInvalidUserId
  | DeleteArticleErrorInvalidSlug
  | DeleteArticleErrorArticleNotFound
  | DeleteArticleErrorDeletePermissionDenied
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

deleteArticle ::
  (ArticleRepository :> es, TxManager :> es) =>
  DeleteArticleCommand ->
  Eff es (Either DeleteArticleError DeleteArticleResult)
deleteArticle command = runErrorNoCallStack $ do
  slug <- mkSlug command.slug `whenNothing` throwError DeleteArticleErrorInvalidSlug
  actorId <- readMaybe (toString command.userId) `whenNothing` throwError DeleteArticleErrorInvalidUserId
  _ <- withTx $ do
    article <- ArticleRepository.findBySlug slug `whenNothingM` throwError DeleteArticleErrorArticleNotFound
    unless (Article.isDeletable article actorId) $
      throwError DeleteArticleErrorDeletePermissionDenied
    ArticleRepository.delete article
  pure $ DeleteArticleResult{slug = unSlug slug}

----------------------------------------------------------------------------------------------------
-- Add Comments to an Article

data AddCommentsCommand = AddCommentsCommand
  { userId :: Text
  , slug :: Text
  , body :: Text
  }
  deriving stock (Show, Eq, Generic)

data AddCommentsResult = AddCommentsResult
  { commentId :: Text
  , createdAt :: UTCTime
  , authorUsername :: Text
  }
  deriving stock (Show, Eq)

data AddCommentsError
  = AddCommentsErrorInvalidUserId
  | AddCommentsErrorInvalidBody
  | AddCommentsErrorArticleNotFound
  | AddCommentsErrorAuthorNotFound
  | AddCommentsErrorInvalidSlug
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

addComments ::
  (IOE :> es, ArticleRepository :> es, UserRepository :> es, CommentRepository :> es, TxManager :> es) =>
  AddCommentsCommand ->
  Eff es (Either AddCommentsError AddCommentsResult)
addComments command = runErrorNoCallStack $ do
  slug <- mkSlug command.slug `whenNothing` throwError AddCommentsErrorInvalidSlug
  authorId <- readMaybe (toString command.userId) `whenNothing` throwError AddCommentsErrorInvalidUserId
  body <- mkCommentBody command.body `whenNothing` throwError AddCommentsErrorInvalidBody
  commentId <- liftIO getULID
  createdAt <- liftIO getCurrentTime
  author <- withTx $ do
    author <- UserRepository.findById authorId `whenNothingM` throwError AddCommentsErrorAuthorNotFound
    article <- ArticleRepository.findBySlug slug `whenNothingM` throwError AddCommentsErrorArticleNotFound
    let comment =
          mkComment
            commentId
            body
            createdAt
            authorId
            article.articleId
    _ <- CommentRepository.save comment
    pure author
  pure $
    AddCommentsResult
      { commentId = show commentId
      , createdAt = createdAt
      , authorUsername = author.username.unUsername.unBoundedText
      }

----------------------------------------------------------------------------------------------------
-- Delete Comment

data DeleteCommentCommand = DeleteCommentCommand
  { userId :: Text
  , slug :: Text
  , commentId :: Text
  }
  deriving stock (Show, Eq, Generic)

data DeleteCommentResult = DeleteCommentResult
  { slug :: Text
  , commentId :: Text
  }
  deriving stock (Show, Eq)

data DeleteCommentError
  = DeleteCommentErrorInvalidUserId
  | DeleteCommentErrorInvalidSlug
  | DeleteCommentErrorInvalidCommentId
  | DeleteCommentErrorArticleNotFound
  | DeleteCommentErrorCommentNotFound
  | DeleteCommentErrorDeletePermissionDenied
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

deleteComment ::
  (ArticleRepository :> es, CommentRepository :> es, TxManager :> es) =>
  DeleteCommentCommand ->
  Eff es (Either DeleteCommentError DeleteCommentResult)
deleteComment command = runErrorNoCallStack $ do
  slug <- mkSlug command.slug `whenNothing` throwError DeleteCommentErrorInvalidSlug
  actorId <- readMaybe (toString command.userId) `whenNothing` throwError DeleteCommentErrorInvalidUserId
  commentId <- readMaybe (toString command.commentId) `whenNothing` throwError DeleteCommentErrorInvalidCommentId
  _ <- withTx $ do
    _ <- ArticleRepository.findBySlug slug `whenNothingM` throwError DeleteCommentErrorArticleNotFound
    comment <- CommentRepository.findById commentId `whenNothingM` throwError DeleteCommentErrorCommentNotFound
    unless (Comment.isDeletable comment actorId) $
      throwError DeleteCommentErrorDeletePermissionDenied
    CommentRepository.delete comment
  pure $
    DeleteCommentResult
      { slug = unSlug slug
      , commentId = show commentId
      }

----------------------------------------------------------------------------------------------------
-- Favorite Article

data FavoriteArticleCommand = FavoriteArticleCommand
  { userId :: Text
  , slug :: Text
  }
  deriving stock (Show, Eq, Generic)

data FavoriteArticleResult = FavoriteArticleResult
  { slug :: Text
  , title :: Text
  , description :: Text
  , body :: Text
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  , tags :: [Text]
  , favoritesCount :: Int
  , authorUsername :: Text
  }
  deriving stock (Show, Eq)

data FavoriteArticleError
  = FavoriteArticleErrorInvalidUserId
  | FavoriteArticleErrorInvalidSlug
  | FavoriteArticleErrorArticleNotFound
  | FavroiteArticleErrorUserNotFound
  | FavoriteArticleErrorAlreadyFavorited
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

favoriteArticle ::
  (IOE :> es, ArticleRepository :> es, FavoriteRepository :> es, UserRepository :> es, TxManager :> es) =>
  FavoriteArticleCommand ->
  Eff es (Either FavoriteArticleError FavoriteArticleResult)
favoriteArticle command = runErrorNoCallStack $ do
  slug <- mkSlug command.slug `whenNothing` throwError FavoriteArticleErrorInvalidSlug
  actorId <- readMaybe (toString command.userId) `whenNothing` throwError FavoriteArticleErrorInvalidUserId
  createdAt <- liftIO getCurrentTime
  (article, actor) <- withTx $ do
    article <- ArticleRepository.findBySlug slug `whenNothingM` throwError FavoriteArticleErrorArticleNotFound
    actor <- UserRepository.findById actorId `whenNothingM` throwError FavroiteArticleErrorUserNotFound
    let favoriteId = mkFavoriteId article.articleId actorId
    let favorite = mkFavorite favoriteId createdAt
    success <- FavoriteRepository.save favorite
    unless success $
      throwError FavoriteArticleErrorAlreadyFavorited
    let article' = Article.increseFavoritesCount article
    _ <- ArticleRepository.save article'
    pure (article', actor)
  pure $
    FavoriteArticleResult
      { slug = unSlug article.slug
      , title = unTitle article.title
      , description = unDescription article.description
      , body = unArticleBody article.body
      , createdAt = article.createdAt
      , updatedAt = article.updatedAt
      , tags = unTag <$> article.tags
      , favoritesCount = article.favoritesCount
      , authorUsername = actor.username.unUsername.unBoundedText
      }

----------------------------------------------------------------------------------------------------
-- Unfavorite Article

data UnfavoriteArticleCommand = UnfavoriteArticleCommand
  { userId :: Text
  , slug :: Text
  }
  deriving stock (Show, Eq, Generic)

data UnfavoriteArticleResult = UnfavoriteArticleResult
  { slug :: Text
  , title :: Text
  , description :: Text
  , body :: Text
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  , tags :: [Text]
  , favoritesCount :: Int
  , authorUsername :: Text
  }
  deriving stock (Show, Eq)

data UnfavoriteArticleError
  = UnfavoriteArticleErrorInvalidUserId
  | UnfavoriteArticleErrorInvalidSlug
  | UnfavoriteArticleErrorArticleNotFound
  | UnfavroiteArticleErrorUserNotFound
  | UnfavroiteArticleErrorIsNotFavorited
  | UnfavoriteArticleErrorNotFavorited
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

unfavoriteArticle ::
  (ArticleRepository :> es, FavoriteRepository :> es, UserRepository :> es, TxManager :> es) =>
  UnfavoriteArticleCommand ->
  Eff es (Either UnfavoriteArticleError UnfavoriteArticleResult)
unfavoriteArticle command = runErrorNoCallStack $ do
  slug <- mkSlug command.slug `whenNothing` throwError UnfavoriteArticleErrorInvalidSlug
  actorId <- readMaybe (toString command.userId) `whenNothing` throwError UnfavoriteArticleErrorInvalidUserId
  (article, actor) <- withTx $ do
    article <- ArticleRepository.findBySlug slug `whenNothingM` throwError UnfavoriteArticleErrorArticleNotFound
    actor <- UserRepository.findById actorId `whenNothingM` throwError UnfavroiteArticleErrorUserNotFound
    let favoriteId = mkFavoriteId article.articleId actorId
    favorite <- FavoriteRepository.findById favoriteId `whenNothingM` throwError UnfavroiteArticleErrorIsNotFavorited
    success <- FavoriteRepository.delete favorite
    unless success $
      throwError UnfavoriteArticleErrorNotFavorited
    let article' = Article.decreseFavoritesCount article
    _ <- ArticleRepository.save article'
    pure (article', actor)
  pure $
    UnfavoriteArticleResult
      { slug = unSlug article.slug
      , title = unTitle article.title
      , description = unDescription article.description
      , body = unArticleBody article.body
      , createdAt = article.createdAt
      , updatedAt = article.updatedAt
      , tags = unTag <$> article.tags
      , favoritesCount = article.favoritesCount
      , authorUsername = actor.username.unUsername.unBoundedText
      }
