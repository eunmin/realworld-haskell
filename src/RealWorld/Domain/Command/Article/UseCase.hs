{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
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
  )
import qualified RealWorld.Domain.Command.Article.Entity.Article as Article
import qualified RealWorld.Domain.Command.Article.Entity.Comment as Comment
import RealWorld.Domain.Command.Article.Value
  ( ArticleBody (unArticleBody),
    Description (unDescription),
    Slug (unSlug),
    Tag (unTag),
    Title (unTitle),
    mkArticleBody,
    mkCommentBody,
    mkDescription,
    mkSlug,
    mkTag,
    mkTitle,
  )
import RealWorld.Domain.Command.User.Entity.User (userUsername)
import RealWorld.Domain.Command.User.Value (Token (..), Username (unUsername))
import RealWorld.Util.BoundedText (BoundedText (unBoundedText))
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
  deriving (Show, Eq)

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
        Article.mkArticle
          articleId
          title
          description
          body
          tags
          createdAt
          authorId
  withTx $ do
    lift $ ArticleRepository.save article
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
    updateArticleResultAuthorUsername :: Text
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
  deriving (Show, Eq)

updateArticle ::
  (MonadIO m, ArticleRepository m, UserRepository m, TokenGateway m, TxManager m) =>
  UpdateArticleCommand ->
  m (Either UpdateArticleError UpdateArticleResult)
updateArticle UpdateArticleCommand {..} = runExceptT $ do
  slug <- mkSlug updateArticleCommandSlug ?? UpdateArticleErrorInvalidSlug
  title <- traverse mkTitle updateArticleCommandTitle ?? UpdateArticleErrorInvalidTitle
  description <- traverse mkDescription updateArticleCommandDescription ?? UpdateArticleErrorInvalidDescription
  body <- traverse mkArticleBody updateArticleCommandBody ?? UpdateArticleErrorInvalidBody
  actorId <- TokenGateway.verify (Token updateArticleCommandToken) !? UpdateArticleErrorInvalidToken
  (article, author) <- withTx $ do
    article <- ArticleRepository.findBySlug slug !? UpdateArticleErrorArticleNotFound
    author <- UserRepository.findById (articleAuthorId article) !? UpdateArticleErrorAuthorNotFound
    article' <- Article.update article actorId title description body ?? UpdateArticleErrorAuthorMismatch
    lift $ ArticleRepository.save article'
    pure (article', author)
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
        updateArticleResultAuthorUsername = unBoundedText . unUsername . userUsername $ author
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
  deriving (Show, Eq)

deleteArticle ::
  (MonadIO m, ArticleRepository m, TokenGateway m, TxManager m) =>
  DeleteArticleCommand ->
  m (Either DeleteArticleError DeleteArticleResult)
deleteArticle DeleteArticleCommand {..} = runExceptT $ do
  slug <- mkSlug deleteArticleCommandSlug ?? DeleteArticleErrorInvalidSlug
  actorId <- TokenGateway.verify (Token deleteArticleCommandToken) !? DeleteArticleErrorInvalidToken
  withTx $ do
    article <- ArticleRepository.findBySlug slug !? DeleteArticleErrorArticleNotFound
    when (Article.isDeletable article actorId)
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
  deriving (Show, Eq)

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
          Comment.mkComment
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
-- Delete Comments

----------------------------------------------------------------------------------------------------
-- Favorite Article

----------------------------------------------------------------------------------------------------
-- Unfavorite Article
