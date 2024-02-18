{-# LANGUAGE UndecidableInstances #-}

module RealWorld.App
  ( main,
    mainDev,
  )
where

import Control.Monad.Catch
import RealWorld.Domain.Repo (Tx (..))
import RealWorld.Domain.User.Gateway.Password (PasswordGateway (..))
import RealWorld.Domain.User.Gateway.Token (TokenGateway (..))
import RealWorld.Domain.User.Repo (UserRepository (..))
import qualified RealWorld.Infra.Component.HttpServer as HttpServerConfig
import qualified RealWorld.Infra.Database.PGQuery as PGQuery
import qualified RealWorld.Infra.Database.PGUserRepository as PGUserRepository
import qualified RealWorld.Infra.Database.Repo as Repo
import qualified RealWorld.Infra.Gateway.JwtToken as JwtTokenGateway
import qualified RealWorld.Infra.Service.BcryptPasswordService as BcryptPasswordService
import qualified RealWorld.Infra.System as System
import RealWorld.Infra.Web.Routes (routes)
import RealWorld.Query.Types (Query (..))
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

instance TokenGateway App where
  generate = JwtTokenGateway.generate
  verify = JwtTokenGateway.verify

instance PasswordGateway App where
  hashPassword = BcryptPasswordService.hashPassword
  isValidPassword = BcryptPasswordService.isValidPassword

instance Tx App where
  withTx = Repo.withTx

instance Query App where
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