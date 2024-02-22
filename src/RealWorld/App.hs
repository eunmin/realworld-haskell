{-# LANGUAGE UndecidableInstances #-}

module RealWorld.App
  ( main,
    mainDev,
  )
where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway (..))
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway (..))
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager (..))
import RealWorld.Domain.Adapter.Repository.ArticleRepository
  ( ArticleRepository (..),
  )
import RealWorld.Domain.Adapter.Repository.CommentRepository
  ( CommentRepository (..),
  )
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository (..))
import RealWorld.Domain.Query.Service (QueryService (..))
import qualified RealWorld.Infra.Component.HttpServer as HttpServerConfig
import qualified RealWorld.Infra.Gateway.BcryptPasswordGateway as BcryptPasswordGateway
import qualified RealWorld.Infra.Gateway.JwtToken as JwtTokenGateway
import qualified RealWorld.Infra.Manager.PgTxManager as PgTxManager
import qualified RealWorld.Infra.Repository.PgArticleRepository as PgArticleRepository
import qualified RealWorld.Infra.Repository.PgCommentRepository as PgCommentRepository
import qualified RealWorld.Infra.Repository.PgQuery as PgQuery
import qualified RealWorld.Infra.Repository.PgUserRepository as PgUserRepository
import qualified RealWorld.Infra.System as System
import RealWorld.Infra.Web.Routes (routes)
import Relude
import Web.Scotty.Trans

newtype App a = App {unApp :: StateT System.State IO a}
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadCatch,
      MonadThrow,
      MonadState System.State,
      MonadMask,
      MonadFail
    )

instance UserRepository App where
  save = PgUserRepository.save
  findById = PgUserRepository.findById
  findByUsername = PgUserRepository.findByUsername
  findByEmail = PgUserRepository.findByEmail
  follow = PgUserRepository.follow
  unfollow = PgUserRepository.unfollow
  hasFollowing = PgUserRepository.hasFollowing

instance ArticleRepository App where
  save = PgArticleRepository.save
  findById = PgArticleRepository.findById
  findBySlug = PgArticleRepository.findBySlug
  delete = PgArticleRepository.delete

instance CommentRepository App where
  save = PgCommentRepository.save
  findById = PgCommentRepository.findById
  delete = PgCommentRepository.delete

instance TokenGateway App where
  generate = JwtTokenGateway.generate
  verify = JwtTokenGateway.verify

instance PasswordGateway App where
  hashPassword = BcryptPasswordGateway.hashPassword
  isValidPassword = BcryptPasswordGateway.isValidPassword

instance TxManager App where
  withTx = PgTxManager.withTx

instance QueryService App where
  getCurrentUser = PgQuery.getCurrentUser
  getProfile = PgQuery.getProfile
  listArticles = PgQuery.listArticles
  feedArticles = PgQuery.feedArticles
  getArticle = PgQuery.getArticle
  getCommentsFromArticle = PgQuery.getCommentsFromArticle
  getTags = PgQuery.getTags

mainWithConfig :: System.Config -> IO ()
mainWithConfig config = do
  let port = config & System.configHttpServer & HttpServerConfig.configPort
  System.withState config $ \state' -> do
    scottyT port (\app -> fst <$> runStateT (unApp app) state') routes

main :: IO ()
main = do
  config <- System.configFromEnv
  mainWithConfig config

mainDev :: IO ()
mainDev = mainWithConfig System.devConfig