module Conduit.App
  ( main,
    mainDev,
  )
where

import Conduit.Domain.User.Gateway.Token (TokenGateway (..))
import Conduit.Domain.User.Repo (UserRepository (..))
import Conduit.Domain.User.Service.Password (PasswordService (..))
import qualified Conduit.Infra.Component.HttpServer as HttpServerConfig
import qualified Conduit.Infra.Database.PGUserRepository as PGUserRepository
import qualified Conduit.Infra.Gateway.JwtToken as JwtTokenGateway
import qualified Conduit.Infra.Service.BcryptPasswordService as BcryptPasswordService
import qualified Conduit.Infra.System as System
import Conduit.Infra.Web.Routes (routes)
import Relude hiding (state)
import Web.Scotty.Trans

newtype App a = App {unApp :: ReaderT System.State IO a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader System.State
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

mainWithConfig :: System.Config -> IO ()
mainWithConfig config = do
  let port = config & System.configHttpServer & HttpServerConfig.configPort
  System.withState config $ \state -> do
    scottyT port (\app -> runReaderT (unApp app) state) routes

main :: IO ()
main = do
  config <- System.configFromEnv
  mainWithConfig config

mainDev :: IO ()
mainDev = mainWithConfig System.devConfig