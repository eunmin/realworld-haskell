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
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository (..))
import RealWorld.Domain.Query.Service (QueryService (..))
import qualified RealWorld.Infra.Component.HttpServer as HttpServerConfig
import qualified RealWorld.Infra.Database.PGArticleRepository as PGArticleRepository
import qualified RealWorld.Infra.Database.PGQuery as PGQuery
import qualified RealWorld.Infra.Database.PGUserRepository as PGUserRepository
import qualified RealWorld.Infra.Database.Repo as Repo
import qualified RealWorld.Infra.Gateway.BcryptPasswordGateway as BcryptPasswordGateway
import qualified RealWorld.Infra.Gateway.JwtToken as JwtTokenGateway
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
  save = PGUserRepository.save
  findById = PGUserRepository.findById
  findByUsername = PGUserRepository.findByUsername
  findByEmail = PGUserRepository.findByEmail
  follow = PGUserRepository.follow
  unfollow = PGUserRepository.unfollow
  hasFollowing = PGUserRepository.hasFollowing

instance ArticleRepository App where
  save = PGArticleRepository.save
  findById = PGArticleRepository.findById
  findBySlug = PGArticleRepository.findBySlug
  delete = PGArticleRepository.delete

instance TokenGateway App where
  generate = JwtTokenGateway.generate
  verify = JwtTokenGateway.verify

instance PasswordGateway App where
  hashPassword = BcryptPasswordGateway.hashPassword
  isValidPassword = BcryptPasswordGateway.isValidPassword

instance TxManager App where
  withTx = Repo.withTx

instance QueryService App where
  getCurrentUser = PGQuery.getCurrentUser
  getProfile = PGQuery.getProfile
  listArticles = PGQuery.listArticles
  feedArticles = PGQuery.feedArticles
  getArticle = PGQuery.getArticle
  getCommentsFromArticle = PGQuery.getCommentsFromArticle
  getTags = PGQuery.getTags

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