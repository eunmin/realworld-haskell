{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module RealWorld.Domain.Command.Article.UseCase where

import Control.Error (throwE)
import Control.Error.Util ((!?), (??))
import Data.Text (unpack)
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
  ( Article
      ( articleAuthorId,
        articleBody,
        articleCreatedAt,
        articleDescription,
        articleFavoritesCount,
        articleId,
        articleSlug,
        articleTags,
        articleTitle,
        articleUpdatedAt
      ),
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
import RealWorld.Domain.Command.User.Entity.User (userUsername)
import RealWorld.Domain.Command.User.Value (Token (..), Username (unUsername))
import RealWorld.Domain.Util.BoundedText (BoundedText (unBoundedText))
import Relude hiding ((??))

----------------------------------------------------------------------------------------------------
-- Create Article

data CreateArticleCommand = CreateArticleCommand
  { createArticleCommandTitle :: Text,
    createArticleCommandDescription :: Text,
    createArticleCommandBody :: Text,
    createArticleCommandTagList :: [Text],
    createArticleCommandToken :: Text
  }
  deriving (Show, Eq, Generic)

data CreateArticleResult = CreateArticleResult
  { createArticleResultSlug :: Text,
    createArticleResultCreatedAt :: UTCTime,
    createArticleResultAuthorUsername :: Text
  }
  deriving (Show, Eq)

data CreateArticleError
  = CreateArticleErrorInvalidTitle
  | CreateArticleErrorInvalidToken
  | CreateArticleErrorInvalidBody
  | CreateArticleErrorInvalidDescription
  | CreateArticleErrorInvalidTag
  | CreateArticleErrorAuthorNotFound
  deriving (Show, Eq, Generic)

createArticle ::
  (MonadIO m, ArticleRepository m, UserRepository m, TokenGateway m, TxManager m) =>
  CreateArticleCommand ->
  m (Either CreateArticleError CreateArticleResult)
createArticle CreateArticleCommand {..} = runExceptT $ do
  articleId <- liftIO getULID
  createdAt <- liftIO getCurrentTime
  authorId <- TokenGateway.verify (Token createArticleCommandToken) !? CreateArticleErrorInvalidToken
  title <- mkTitle createArticleCommandTitle ?? CreateArticleErrorInvalidTitle
  body <- mkArticleBody createArticleCommandBody ?? CreateArticleErrorInvalidBody
  description <- mkDescription createArticleCommandDescription ?? CreateArticleErrorInvalidDescription
  tags <- traverse mkTag createArticleCommandTagList ?? CreateArticleErrorInvalidTag
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
      { createArticleResultSlug = unSlug $ articleSlug article,
        createArticleResultCreatedAt = createdAt,
        createArticleResultAuthorUsername = unBoundedText . unUsername . userUsername $ author
      }

----------------------------------------------------------------------------------------------------
-- Update Article

data UpdateArticleCommand = UpdateArticleCommand
  { updateArticleCommandToken :: Text,
    updateArticleCommandSlug :: Text,
    updateArticleCommandTitle :: Maybe Text,
    updateArticleCommandDescription :: Maybe Text,
    updateArticleCommandBody :: Maybe Text
  }
  deriving (Show, Eq, Generic)

data UpdateArticleResult = UpdateArticleResult
  { updateArticleResultSlug :: Text,
    updateArticleResultTitle :: Text,
    updateArticleResultDescription :: Text,
    updateArticleResultBody :: Text,
    updateArticleResultCreatedAt :: UTCTime,
    updateArticleResultUpdatedAt :: Maybe UTCTime,
    updateArticleResultTags :: [Text],
    updateArticleResultFavoritesCount :: Int,
    updateArticleResultAuthorUsername :: Text,
    updateArticleResultFavorited :: Bool
  }
  deriving (Show, Eq)

data UpdateArticleError
  = UpdateArticleErrorInvalidTitle
  | UpdateArticleErrorInvalidToken
  | UpdateArticleErrorInvalidSlug
  | UpdateArticleErrorArticleNotFound
  | UpdateArticleErrorInvalidBody
  | UpdateArticleErrorInvalidDescription
  | UpdateArticleErrorAuthorNotFound
  | UpdateArticleErrorAuthorMismatch
  deriving (Show, Eq, Generic)

updateArticle ::
  (MonadIO m, ArticleRepository m, UserRepository m, FavoriteRepository m, TokenGateway m, TxManager m) =>
  UpdateArticleCommand ->
  m (Either UpdateArticleError UpdateArticleResult)
updateArticle UpdateArticleCommand {..} = runExceptT $ do
  slug <- mkSlug updateArticleCommandSlug ?? UpdateArticleErrorInvalidSlug
  title <- traverse mkTitle updateArticleCommandTitle ?? UpdateArticleErrorInvalidTitle
  description <- traverse mkDescription updateArticleCommandDescription ?? UpdateArticleErrorInvalidDescription
  body <- traverse mkArticleBody updateArticleCommandBody ?? UpdateArticleErrorInvalidBody
  actorId <- TokenGateway.verify (Token updateArticleCommandToken) !? UpdateArticleErrorInvalidToken
  (article, author, favorited) <- withTx $ do
    article <- ArticleRepository.findBySlug slug !? UpdateArticleErrorArticleNotFound
    author <- UserRepository.findById (articleAuthorId article) !? UpdateArticleErrorAuthorNotFound
    article' <- Article.update article actorId title description body ?? UpdateArticleErrorAuthorMismatch
    _ <- lift $ ArticleRepository.save article'
    favorited <- lift $ FavoriteRepository.findById $ FavoriteId (articleId article) actorId
    pure (article', author, isJust favorited)
  pure
    $ UpdateArticleResult
      { updateArticleResultSlug = unSlug $ articleSlug article,
        updateArticleResultTitle = unTitle . articleTitle $ article,
        updateArticleResultDescription = unDescription . articleDescription $ article,
        updateArticleResultBody = unArticleBody . articleBody $ article,
        updateArticleResultCreatedAt = articleCreatedAt article,
        updateArticleResultUpdatedAt = articleUpdatedAt article,
        updateArticleResultTags = unTag <$> articleTags article,
        updateArticleResultFavoritesCount = articleFavoritesCount article,
        updateArticleResultAuthorUsername = unBoundedText . unUsername . userUsername $ author,
        updateArticleResultFavorited = favorited
      }

----------------------------------------------------------------------------------------------------
-- Delete Article

data DeleteArticleCommand = DeleteArticleCommand
  { deleteArticleCommandToken :: Text,
    deleteArticleCommandSlug :: Text
  }
  deriving (Show, Eq, Generic)

data DeleteArticleResult = DeleteArticleResult
  { deleteArticleResultSlug :: Text
  }
  deriving (Show, Eq)

data DeleteArticleError
  = DeleteArticleErrorInvalidToken
  | DeleteArticleErrorInvalidSlug
  | DeleteArticleErrorArticleNotFound
  | DeleteArticleErrorAuthorMismatch
  deriving (Show, Eq, Generic)

deleteArticle ::
  (MonadIO m, ArticleRepository m, TokenGateway m, TxManager m) =>
  DeleteArticleCommand ->
  m (Either DeleteArticleError DeleteArticleResult)
deleteArticle DeleteArticleCommand {..} = runExceptT $ do
  slug <- mkSlug deleteArticleCommandSlug ?? DeleteArticleErrorInvalidSlug
  actorId <- TokenGateway.verify (Token deleteArticleCommandToken) !? DeleteArticleErrorInvalidToken
  _ <- withTx $ do
    article <- ArticleRepository.findBySlug slug !? DeleteArticleErrorArticleNotFound
    unless (Article.isDeletable article actorId)
      $ throwE DeleteArticleErrorAuthorMismatch
    lift $ ArticleRepository.delete article
  pure $ DeleteArticleResult {deleteArticleResultSlug = unSlug slug}

----------------------------------------------------------------------------------------------------
-- Add Comments to an Article

data AddCommentsCommand = AddCommentsCommand
  { addCommentsCommandToken :: Text,
    addCommentsCommandSlug :: Text,
    addCommentsCommandBody :: Text
  }
  deriving (Show, Eq, Generic)

data AddCommentsResult = AddCommentsResult
  { addCommentsResultCommentId :: Text,
    addCommentsResultCreatedAt :: UTCTime,
    addCommentsResultAuthorUsername :: Text
  }
  deriving (Show, Eq)

data AddCommentsError
  = AddCommentsErrorInvalidToken
  | AddCommentsErrorInvalidBody
  | AddCommentsErrorArticleNotFound
  | AddCommentsErrorAuthorNotFound
  | AddCommentsErrorInvalidSlug
  deriving (Show, Eq, Generic)

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
addComments AddCommentsCommand {..} = runExceptT $ do
  slug <- mkSlug addCommentsCommandSlug ?? AddCommentsErrorInvalidSlug
  authorId <- TokenGateway.verify (Token addCommentsCommandToken) !? AddCommentsErrorInvalidToken
  body <- mkCommentBody addCommentsCommandBody ?? AddCommentsErrorInvalidBody
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
            (articleId article)
    _ <- lift $ CommentRepository.save comment
    pure author
  pure
    $ AddCommentsResult
      { addCommentsResultCommentId = show commentId,
        addCommentsResultCreatedAt = createdAt,
        addCommentsResultAuthorUsername = unBoundedText . unUsername . userUsername $ author
      }

----------------------------------------------------------------------------------------------------
-- Delete Comment

data DeleteCommentCommand = DeleteCommentCommand
  { deleteCommentCommandToken :: Text,
    deleteCommentCommandSlug :: Text,
    deleteCommentCommandCommentId :: Text
  }
  deriving (Show, Eq, Generic)

data DeleteCommentResult = DeleteCommentResult
  { deleteCommentResultSlug :: Text,
    deleteCommentResultCommentId :: Text
  }
  deriving (Show, Eq)

data DeleteCommentError
  = DeleteCommentErrorInvalidToken
  | DeleteCommentErrorInvalidSlug
  | DeleteCommentErrorInvalidCommentId
  | DeleteCommentErrorArticleNotFound
  | DeleteCommentErrorCommentNotFound
  | DeleteCommentErrorAuthorMismatch
  deriving (Show, Eq, Generic)

deleteComment ::
  (MonadIO m, ArticleRepository m, TokenGateway m, CommentRepository m, TxManager m) =>
  DeleteCommentCommand ->
  m (Either DeleteCommentError DeleteCommentResult)
deleteComment DeleteCommentCommand {..} = runExceptT $ do
  slug <- mkSlug deleteCommentCommandSlug ?? DeleteCommentErrorInvalidSlug
  actorId <- TokenGateway.verify (Token deleteCommentCommandToken) !? DeleteCommentErrorInvalidToken
  commentId <- readMaybe (unpack deleteCommentCommandCommentId) ?? DeleteCommentErrorInvalidCommentId
  _ <- withTx $ do
    _ <- ArticleRepository.findBySlug slug !? DeleteCommentErrorArticleNotFound
    comment <- CommentRepository.findById commentId !? DeleteCommentErrorCommentNotFound
    unless (Comment.isDeletable comment actorId)
      $ throwE DeleteCommentErrorAuthorMismatch
    lift $ CommentRepository.delete comment
  pure
    $ DeleteCommentResult
      { deleteCommentResultSlug = unSlug slug,
        deleteCommentResultCommentId = show commentId
      }

----------------------------------------------------------------------------------------------------
-- Favorite Article

data FavoriteArticleCommand = FavoriteArticleCommand
  { favoriteArticleToken :: Text,
    favoriteArticleSlug :: Text
  }
  deriving (Show, Eq, Generic)

data FavoriteArticleResult = FavoriteArticleResult
  { favoriteArticleResultSlug :: Text,
    favoriteArticleResultTitle :: Text,
    favoriteArticleResultDescription :: Text,
    favoriteArticleResultBody :: Text,
    favoriteArticleResultCreatedAt :: UTCTime,
    favoriteArticleResultUpdatedAt :: Maybe UTCTime,
    favoriteArticleResultTags :: [Text],
    favoriteArticleResultFavoritesCount :: Int,
    favoriteArticleResultAuthorUsername :: Text
  }
  deriving (Show, Eq)

data FavoriteArticleError
  = FavoriteArticleErrorInvalidToken
  | FavoriteArticleErrorInvalidSlug
  | FavoriteArticleErrorArticleNotFound
  | FavroiteArticleErrorUserNotFound
  deriving (Show, Eq, Generic)

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
favoriteArticle FavoriteArticleCommand {..} = runExceptT $ do
  slug <- mkSlug favoriteArticleSlug ?? FavoriteArticleErrorInvalidSlug
  actorId <- TokenGateway.verify (Token favoriteArticleToken) !? FavoriteArticleErrorInvalidToken
  createdAt <- liftIO getCurrentTime
  (article, actor) <- withTx $ do
    article <- ArticleRepository.findBySlug slug !? FavoriteArticleErrorArticleNotFound
    actor <- UserRepository.findById actorId !? FavroiteArticleErrorUserNotFound
    let favoriteId = mkFavoriteId (articleId article) actorId
    let favorite = mkFavorite favoriteId createdAt
    success <- lift $ FavoriteRepository.save favorite
    if success
      then do
        let article' = Article.increseFavoritesCount article
        _ <- lift $ ArticleRepository.save article'
        pure (article', actor)
      else pure (article, actor)
  pure
    $ FavoriteArticleResult
      { favoriteArticleResultSlug = unSlug $ articleSlug article,
        favoriteArticleResultTitle = unTitle . articleTitle $ article,
        favoriteArticleResultDescription = unDescription . articleDescription $ article,
        favoriteArticleResultBody = unArticleBody . articleBody $ article,
        favoriteArticleResultCreatedAt = articleCreatedAt article,
        favoriteArticleResultUpdatedAt = articleUpdatedAt article,
        favoriteArticleResultTags = unTag <$> articleTags article,
        favoriteArticleResultFavoritesCount = articleFavoritesCount article,
        favoriteArticleResultAuthorUsername = unBoundedText . unUsername . userUsername $ actor
      }

----------------------------------------------------------------------------------------------------
-- Unfavorite Article

data UnfavoriteArticleCommand = UnfavoriteArticleCommand
  { unfavoriteArticleToken :: Text,
    unfavoriteArticleSlug :: Text
  }
  deriving (Show, Eq, Generic)

data UnfavoriteArticleResult = UnfavoriteArticleResult
  { unfavoriteArticleResultSlug :: Text,
    unfavoriteArticleResultTitle :: Text,
    unfavoriteArticleResultDescription :: Text,
    unfavoriteArticleResultBody :: Text,
    unfavoriteArticleResultCreatedAt :: UTCTime,
    unfavoriteArticleResultUpdatedAt :: Maybe UTCTime,
    unfavoriteArticleResultTags :: [Text],
    unfavoriteArticleResultFavoritesCount :: Int,
    unfavoriteArticleResultAuthorUsername :: Text
  }
  deriving (Show, Eq)

data UnfavoriteArticleError
  = UnfavoriteArticleErrorInvalidToken
  | UnfavoriteArticleErrorInvalidSlug
  | UnfavoriteArticleErrorArticleNotFound
  | UnfavroiteArticleErrorUserNotFound
  | UnfavroiteArticleErrorIsNotFavorited
  deriving (Show, Eq, Generic)

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
unfavoriteArticle UnfavoriteArticleCommand {..} = runExceptT $ do
  slug <- mkSlug unfavoriteArticleSlug ?? UnfavoriteArticleErrorInvalidSlug
  actorId <- TokenGateway.verify (Token unfavoriteArticleToken) !? UnfavoriteArticleErrorInvalidToken
  (article, actor) <- withTx $ do
    article <- ArticleRepository.findBySlug slug !? UnfavoriteArticleErrorArticleNotFound
    actor <- UserRepository.findById actorId !? UnfavroiteArticleErrorUserNotFound
    let favoriteId = mkFavoriteId (articleId article) actorId
    favorite <- FavoriteRepository.findById favoriteId !? UnfavroiteArticleErrorIsNotFavorited
    success <- lift $ FavoriteRepository.delete favorite
    if success
      then do
        let article' = Article.decreseFavoritesCount article
        _ <- lift $ ArticleRepository.save article'
        pure (article', actor)
      else pure (article, actor)
  pure
    $ UnfavoriteArticleResult
      { unfavoriteArticleResultSlug = unSlug $ articleSlug article,
        unfavoriteArticleResultTitle = unTitle . articleTitle $ article,
        unfavoriteArticleResultDescription = unDescription . articleDescription $ article,
        unfavoriteArticleResultBody = unArticleBody . articleBody $ article,
        unfavoriteArticleResultCreatedAt = articleCreatedAt article,
        unfavoriteArticleResultUpdatedAt = articleUpdatedAt article,
        unfavoriteArticleResultTags = unTag <$> articleTags article,
        unfavoriteArticleResultFavoritesCount = articleFavoritesCount article,
        unfavoriteArticleResultAuthorUsername = unBoundedText . unUsername . userUsername $ actor
      }