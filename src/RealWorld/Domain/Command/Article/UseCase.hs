{-# LANGUAGE NoFieldSelectors #-}

module RealWorld.Domain.Command.Article.UseCase where

import Control.Error (throwE)
import Control.Error.Util ((!?), (??))
import Data.Time (UTCTime, getCurrentTime)
import Data.ULID (getULID)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import qualified RealWorld.Domain.Adapter.Gateway.TokenGateway as TokenGateway
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager (withTx))
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository (..))
import qualified RealWorld.Domain.Adapter.Repository.ArticleRepository as ArticleRepository
import RealWorld.Domain.Adapter.Repository.CommentRepository (CommentRepository)
import qualified RealWorld.Domain.Adapter.Repository.CommentRepository as CommentRepository
import RealWorld.Domain.Adapter.Repository.FavoriteRepository (FavoriteRepository)
import qualified RealWorld.Domain.Adapter.Repository.FavoriteRepository as FavoriteRepository
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import qualified RealWorld.Domain.Adapter.Repository.UserRepository as UserRepository
import RealWorld.Domain.Command.Article.Entity.Article
  ( Article (..),
    mkArticle,
  )
import qualified RealWorld.Domain.Command.Article.Entity.Article as Article
import RealWorld.Domain.Command.Article.Entity.Comment (mkComment)
import qualified RealWorld.Domain.Command.Article.Entity.Comment as Comment
import RealWorld.Domain.Command.Article.Entity.Favorite (mkFavorite)
import RealWorld.Domain.Command.Article.Value
  ( ArticleBody (unArticleBody),
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
import RealWorld.Domain.Command.User.Value (Token (..), Username (..))
import RealWorld.Domain.Util.BoundedText (BoundedText (..))
import Relude hiding ((??))

----------------------------------------------------------------------------------------------------
-- Create Article

data CreateArticleCommand = CreateArticleCommand
  { title :: Text,
    description :: Text,
    body :: Text,
    tagList :: [Text],
    token :: Text
  }
  deriving stock (Show, Eq, Generic)

data CreateArticleResult = CreateArticleResult
  { slug :: Text,
    createdAt :: UTCTime,
    authorUsername :: Text
  }
  deriving stock (Show, Eq)

data CreateArticleError
  = CreateArticleErrorInvalidTitle
  | CreateArticleErrorInvalidToken
  | CreateArticleErrorInvalidBody
  | CreateArticleErrorInvalidDescription
  | CreateArticleErrorInvalidTag
  | CreateArticleErrorAuthorNotFound
  deriving stock (Show, Eq, Generic)

createArticle ::
  (MonadIO m, ArticleRepository m, UserRepository m, TokenGateway m, TxManager m) =>
  CreateArticleCommand ->
  m (Either CreateArticleError CreateArticleResult)
createArticle command = runExceptT $ do
  articleId <- liftIO getULID
  createdAt <- liftIO getCurrentTime
  authorId <- TokenGateway.verify (Token command.token) !? CreateArticleErrorInvalidToken
  title <- mkTitle command.title ?? CreateArticleErrorInvalidTitle
  body <- mkArticleBody command.body ?? CreateArticleErrorInvalidBody
  description <- mkDescription command.description ?? CreateArticleErrorInvalidDescription
  tags <- traverse mkTag command.tagList ?? CreateArticleErrorInvalidTag
  author <- UserRepository.findById authorId !? CreateArticleErrorAuthorNotFound
  let article =
        mkArticle
          articleId
          title
          description
          body
          tags
          createdAt
          authorId
  _ <- withTx $ lift $ ArticleRepository.save article
  pure
    $ CreateArticleResult
      { slug = unSlug article.slug,
        createdAt = createdAt,
        authorUsername = author.username.unUsername.unBoundedText
      }

----------------------------------------------------------------------------------------------------
-- Update Article

data UpdateArticleCommand = UpdateArticleCommand
  { token :: Text,
    slug :: Text,
    title :: Maybe Text,
    description :: Maybe Text,
    body :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

data UpdateArticleResult = UpdateArticleResult
  { slug :: Text,
    title :: Text,
    description :: Text,
    body :: Text,
    createdAt :: UTCTime,
    updatedAt :: Maybe UTCTime,
    tags :: [Text],
    favoritesCount :: Int,
    authorUsername :: Text,
    favorited :: Bool
  }
  deriving stock (Show, Eq)

data UpdateArticleError
  = UpdateArticleErrorInvalidTitle
  | UpdateArticleErrorInvalidToken
  | UpdateArticleErrorInvalidSlug
  | UpdateArticleErrorArticleNotFound
  | UpdateArticleErrorInvalidBody
  | UpdateArticleErrorInvalidDescription
  | UpdateArticleErrorAuthorNotFound
  | UpdateArticleErrorEditPermissionDenied
  deriving stock (Show, Eq, Generic)

updateArticle ::
  (MonadIO m, ArticleRepository m, UserRepository m, FavoriteRepository m, TokenGateway m, TxManager m) =>
  UpdateArticleCommand ->
  m (Either UpdateArticleError UpdateArticleResult)
updateArticle command = runExceptT $ do
  slug <- mkSlug command.slug ?? UpdateArticleErrorInvalidSlug
  title <- traverse mkTitle command.title ?? UpdateArticleErrorInvalidTitle
  description <- traverse mkDescription command.description ?? UpdateArticleErrorInvalidDescription
  body <- traverse mkArticleBody command.body ?? UpdateArticleErrorInvalidBody
  actorId <- TokenGateway.verify (Token command.token) !? UpdateArticleErrorInvalidToken
  (article, author, favorited) <- withTx $ do
    article <- ArticleRepository.findBySlug slug !? UpdateArticleErrorArticleNotFound
    author <- UserRepository.findById article.authorId !? UpdateArticleErrorAuthorNotFound
    unless (Article.isEditable article actorId)
      $ throwE UpdateArticleErrorEditPermissionDenied
    let article' = Article.update article title description body
    _ <- lift $ ArticleRepository.save article'
    favorited <- lift $ FavoriteRepository.findById (FavoriteId article.articleId actorId)
    pure (article', author, isJust favorited)
  pure
    $ UpdateArticleResult
      { slug = unSlug article.slug,
        title = unTitle article.title,
        description = unDescription article.description,
        body = unArticleBody article.body,
        createdAt = article.createdAt,
        updatedAt = article.updatedAt,
        tags = unTag <$> article.tags,
        favoritesCount = article.favoritesCount,
        authorUsername = author.username.unUsername.unBoundedText,
        favorited = favorited
      }

----------------------------------------------------------------------------------------------------
-- Delete Article

data DeleteArticleCommand = DeleteArticleCommand
  { token :: Text,
    slug :: Text
  }
  deriving stock (Show, Eq, Generic)

data DeleteArticleResult = DeleteArticleResult
  { slug :: Text
  }
  deriving stock (Show, Eq)

data DeleteArticleError
  = DeleteArticleErrorInvalidToken
  | DeleteArticleErrorInvalidSlug
  | DeleteArticleErrorArticleNotFound
  | DeleteArticleErrorDeletePermissionDenied
  deriving stock (Show, Eq, Generic)

deleteArticle ::
  (MonadIO m, ArticleRepository m, TokenGateway m, TxManager m) =>
  DeleteArticleCommand ->
  m (Either DeleteArticleError DeleteArticleResult)
deleteArticle command = runExceptT $ do
  slug <- mkSlug command.slug ?? DeleteArticleErrorInvalidSlug
  actorId <- TokenGateway.verify (Token command.token) !? DeleteArticleErrorInvalidToken
  _ <- withTx $ do
    article <- ArticleRepository.findBySlug slug !? DeleteArticleErrorArticleNotFound
    unless (Article.isDeletable article actorId)
      $ throwE DeleteArticleErrorDeletePermissionDenied
    lift $ ArticleRepository.delete article
  pure $ DeleteArticleResult {slug = unSlug slug}

----------------------------------------------------------------------------------------------------
-- Add Comments to an Article

data AddCommentsCommand = AddCommentsCommand
  { token :: Text,
    slug :: Text,
    body :: Text
  }
  deriving stock (Show, Eq, Generic)

data AddCommentsResult = AddCommentsResult
  { commentId :: Text,
    createdAt :: UTCTime,
    authorUsername :: Text
  }
  deriving stock (Show, Eq)

data AddCommentsError
  = AddCommentsErrorInvalidToken
  | AddCommentsErrorInvalidBody
  | AddCommentsErrorArticleNotFound
  | AddCommentsErrorAuthorNotFound
  | AddCommentsErrorInvalidSlug
  deriving stock (Show, Eq, Generic)

addComments ::
  ( MonadIO m,
    ArticleRepository m,
    UserRepository m,
    TokenGateway m,
    CommentRepository m,
    TxManager m
  ) =>
  AddCommentsCommand ->
  m (Either AddCommentsError AddCommentsResult)
addComments command = runExceptT $ do
  slug <- mkSlug command.slug ?? AddCommentsErrorInvalidSlug
  authorId <- TokenGateway.verify (Token command.token) !? AddCommentsErrorInvalidToken
  body <- mkCommentBody command.body ?? AddCommentsErrorInvalidBody
  commentId <- liftIO getULID
  createdAt <- liftIO getCurrentTime
  author <- withTx $ do
    author <- UserRepository.findById authorId !? AddCommentsErrorAuthorNotFound
    article <- ArticleRepository.findBySlug slug !? AddCommentsErrorArticleNotFound
    let comment =
          mkComment
            commentId
            body
            createdAt
            authorId
            article.articleId
    _ <- lift $ CommentRepository.save comment
    pure author
  pure
    $ AddCommentsResult
      { commentId = show commentId,
        createdAt = createdAt,
        authorUsername = author.username.unUsername.unBoundedText
      }

----------------------------------------------------------------------------------------------------
-- Delete Comment

data DeleteCommentCommand = DeleteCommentCommand
  { token :: Text,
    slug :: Text,
    commentId :: Text
  }
  deriving stock (Show, Eq, Generic)

data DeleteCommentResult = DeleteCommentResult
  { slug :: Text,
    commentId :: Text
  }
  deriving stock (Show, Eq)

data DeleteCommentError
  = DeleteCommentErrorInvalidToken
  | DeleteCommentErrorInvalidSlug
  | DeleteCommentErrorInvalidCommentId
  | DeleteCommentErrorArticleNotFound
  | DeleteCommentErrorCommentNotFound
  | DeleteCommentErrorDeletePermissionDenied
  deriving stock (Show, Eq, Generic)

deleteComment ::
  (MonadIO m, ArticleRepository m, TokenGateway m, CommentRepository m, TxManager m) =>
  DeleteCommentCommand ->
  m (Either DeleteCommentError DeleteCommentResult)
deleteComment command = runExceptT $ do
  slug <- mkSlug command.slug ?? DeleteCommentErrorInvalidSlug
  actorId <- TokenGateway.verify (Token command.token) !? DeleteCommentErrorInvalidToken
  commentId <- readMaybe (toString command.commentId) ?? DeleteCommentErrorInvalidCommentId
  _ <- withTx $ do
    _ <- ArticleRepository.findBySlug slug !? DeleteCommentErrorArticleNotFound
    comment <- CommentRepository.findById commentId !? DeleteCommentErrorCommentNotFound
    unless (Comment.isDeletable comment actorId)
      $ throwE DeleteCommentErrorDeletePermissionDenied
    lift $ CommentRepository.delete comment
  pure
    $ DeleteCommentResult
      { slug = unSlug slug,
        commentId = show commentId
      }

----------------------------------------------------------------------------------------------------
-- Favorite Article

data FavoriteArticleCommand = FavoriteArticleCommand
  { token :: Text,
    slug :: Text
  }
  deriving stock (Show, Eq, Generic)

data FavoriteArticleResult = FavoriteArticleResult
  { slug :: Text,
    title :: Text,
    description :: Text,
    body :: Text,
    createdAt :: UTCTime,
    updatedAt :: Maybe UTCTime,
    tags :: [Text],
    favoritesCount :: Int,
    authorUsername :: Text
  }
  deriving stock (Show, Eq)

data FavoriteArticleError
  = FavoriteArticleErrorInvalidToken
  | FavoriteArticleErrorInvalidSlug
  | FavoriteArticleErrorArticleNotFound
  | FavroiteArticleErrorUserNotFound
  | FavoriteArticleErrorAlreadyFavorited
  deriving stock (Show, Eq, Generic)

favoriteArticle ::
  ( MonadIO m,
    ArticleRepository m,
    FavoriteRepository m,
    UserRepository m,
    TokenGateway m,
    TxManager m
  ) =>
  FavoriteArticleCommand ->
  m (Either FavoriteArticleError FavoriteArticleResult)
favoriteArticle command = runExceptT $ do
  slug <- mkSlug command.slug ?? FavoriteArticleErrorInvalidSlug
  actorId <- TokenGateway.verify (Token command.token) !? FavoriteArticleErrorInvalidToken
  createdAt <- liftIO getCurrentTime
  (article, actor) <- withTx $ do
    article <- ArticleRepository.findBySlug slug !? FavoriteArticleErrorArticleNotFound
    actor <- UserRepository.findById actorId !? FavroiteArticleErrorUserNotFound
    let favoriteId = mkFavoriteId article.articleId actorId
    let favorite = mkFavorite favoriteId createdAt
    success <- lift $ FavoriteRepository.save favorite
    unless success
      $ throwE FavoriteArticleErrorAlreadyFavorited
    let article' = Article.increseFavoritesCount article
    _ <- lift $ ArticleRepository.save article'
    pure (article', actor)
  pure
    $ FavoriteArticleResult
      { slug = unSlug article.slug,
        title = unTitle article.title,
        description = unDescription article.description,
        body = unArticleBody article.body,
        createdAt = article.createdAt,
        updatedAt = article.updatedAt,
        tags = unTag <$> article.tags,
        favoritesCount = article.favoritesCount,
        authorUsername = actor.username.unUsername.unBoundedText
      }

----------------------------------------------------------------------------------------------------
-- Unfavorite Article

data UnfavoriteArticleCommand = UnfavoriteArticleCommand
  { token :: Text,
    slug :: Text
  }
  deriving stock (Show, Eq, Generic)

data UnfavoriteArticleResult = UnfavoriteArticleResult
  { slug :: Text,
    title :: Text,
    description :: Text,
    body :: Text,
    createdAt :: UTCTime,
    updatedAt :: Maybe UTCTime,
    tags :: [Text],
    favoritesCount :: Int,
    authorUsername :: Text
  }
  deriving stock (Show, Eq)

data UnfavoriteArticleError
  = UnfavoriteArticleErrorInvalidToken
  | UnfavoriteArticleErrorInvalidSlug
  | UnfavoriteArticleErrorArticleNotFound
  | UnfavroiteArticleErrorUserNotFound
  | UnfavroiteArticleErrorIsNotFavorited
  | UnfavoriteArticleErrorNotFavorited
  deriving stock (Show, Eq, Generic)

unfavoriteArticle ::
  ( MonadIO m,
    ArticleRepository m,
    FavoriteRepository m,
    UserRepository m,
    TokenGateway m,
    TxManager m
  ) =>
  UnfavoriteArticleCommand ->
  m (Either UnfavoriteArticleError UnfavoriteArticleResult)
unfavoriteArticle command = runExceptT $ do
  slug <- mkSlug command.slug ?? UnfavoriteArticleErrorInvalidSlug
  actorId <- TokenGateway.verify (Token command.token) !? UnfavoriteArticleErrorInvalidToken
  (article, actor) <- withTx $ do
    article <- ArticleRepository.findBySlug slug !? UnfavoriteArticleErrorArticleNotFound
    actor <- UserRepository.findById actorId !? UnfavroiteArticleErrorUserNotFound
    let favoriteId = mkFavoriteId article.articleId actorId
    favorite <- FavoriteRepository.findById favoriteId !? UnfavroiteArticleErrorIsNotFavorited
    success <- lift $ FavoriteRepository.delete favorite
    unless success
      $ throwE UnfavoriteArticleErrorNotFavorited
    let article' = Article.decreseFavoritesCount article
    _ <- lift $ ArticleRepository.save article'
    pure (article', actor)
  pure
    $ UnfavoriteArticleResult
      { slug = unSlug article.slug,
        title = unTitle article.title,
        description = unDescription article.description,
        body = unArticleBody article.body,
        createdAt = article.createdAt,
        updatedAt = article.updatedAt,
        tags = unTag <$> article.tags,
        favoritesCount = article.favoritesCount,
        authorUsername = actor.username.unUsername.unBoundedText
      }
