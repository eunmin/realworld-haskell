module RealWorld.Domain.Command.Article.UseCase where

import Control.Error.Util ((!?), (??))
import Data.Time (UTCTime, getCurrentTime)
import Data.ULID (getULID)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import qualified RealWorld.Domain.Adapter.Gateway.TokenGateway as TokenGateway
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager (withTx))
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository (..))
import qualified RealWorld.Domain.Adapter.Repository.ArticleRepository as ArticleRepository
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import qualified RealWorld.Domain.Adapter.Repository.UserRepository as UserRepository
import RealWorld.Domain.Command.Article.Entity.Article (Article (articleAuthorId, articleSlug))
import qualified RealWorld.Domain.Command.Article.Entity.Article as Article
import RealWorld.Domain.Command.Article.Value
  ( Slug (unSlug),
    mkBody,
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
  body <- mkBody createArticleCommandBody ?? CreateArticleErrorInvalidBody
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
  body <- traverse mkBody updateArticleCommandBody ?? UpdateArticleErrorInvalidBody
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
        updateArticleResultAuthorUsername = unBoundedText . unUsername . userUsername $ author
      }

----------------------------------------------------------------------------------------------------
-- Delete Article

----------------------------------------------------------------------------------------------------
-- Add Comments to an Article

----------------------------------------------------------------------------------------------------
-- Delete Comments

----------------------------------------------------------------------------------------------------
-- Favorite Article

----------------------------------------------------------------------------------------------------
-- Unfavorite Article
