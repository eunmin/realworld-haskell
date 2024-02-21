module RealWorld.Domain.Command.Article.UseCase where

import Control.Error.Util ((!?), (??))
import Data.Time (UTCTime, getCurrentTime)
import Data.ULID (ULID, getULID)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway)
import qualified RealWorld.Domain.Adapter.Gateway.TokenGateway as TokenGateway
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager (withTx))
import RealWorld.Domain.Adapter.Repository.ArticleRepository (ArticleRepository (..))
import qualified RealWorld.Domain.Adapter.Repository.ArticleRepository as ArticleRepository
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import qualified RealWorld.Domain.Adapter.Repository.UserRepository as UserRepository
import RealWorld.Domain.Command.Article.Entity.Article (Article (articleSlug))
import qualified RealWorld.Domain.Command.Article.Entity.Article as Article
import RealWorld.Domain.Command.Article.Value
import RealWorld.Domain.Command.User.Entity.User (User, userUsername)
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