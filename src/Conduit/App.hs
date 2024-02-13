{-# LANGUAGE UndecidableInstances #-}

module Conduit.App
  ( main,
    mainDev,
  )
where

import Conduit.Domain.Repo (Tx (..))
import Conduit.Domain.User.Gateway.Token (TokenGateway (..))
import Conduit.Domain.User.Repo (UserRepository (..))
import Conduit.Domain.User.Service.Password (PasswordService (..))
import qualified Conduit.Infra.Component.HttpServer as HttpServerConfig
import qualified Conduit.Infra.Database.PGUserRepository as PGUserRepository
import qualified Conduit.Infra.Database.Repo as Repo
import qualified Conduit.Infra.Gateway.JwtToken as JwtTokenGateway
import qualified Conduit.Infra.Service.BcryptPasswordService as BcryptPasswordService
import qualified Conduit.Infra.System as System
import Conduit.Infra.Web.Routes (routes)
import Control.Monad.Catch
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
      MonadMask
    )

instance UserRepository App where
  create = PGUserRepository.create
  findById = PGUserRepository.findById
  findByUsername = PGUserRepository.findByUsername
  findByEmail = PGUserRepository.findByEmail

instance TokenGateway App where
  generate = JwtTokenGateway.generate
  verify = JwtTokenGateway.verify

instance PasswordService App where
  hashPassword = BcryptPasswordService.hashPassword
  isValidPassword = BcryptPasswordService.isValidPassword

instance Tx App where
  withTx = Repo.withTx

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