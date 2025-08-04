{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module RealWorld.App (
  main,
)
where

import Control.Exception (bracket)
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Dynamic (Error, runErrorNoCallStack)
import Effectful.Katip (KatipE, runKatipContextE)
import Effectful.Reader.Dynamic (Reader, runReader)
import Katip (
  ColorStrategy (ColorIfTerminal),
  LogEnv,
  Severity (InfoS),
  Verbosity (V2),
  closeScribes,
  defaultScribeSettings,
  initLogEnv,
  jsonFormat,
  mkHandleScribeWithFormatter,
  permitItem,
  registerScribe,
 )
import qualified Network.Wai.Handler.Warp as Warp
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway (..))
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway (..))
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager (..))
import RealWorld.Domain.Adapter.Repository.ArticleRepository (
  ArticleRepository (..),
 )
import RealWorld.Domain.Adapter.Repository.CommentRepository (
  CommentRepository (..),
 )
import RealWorld.Domain.Adapter.Repository.FavoriteRepository (FavoriteRepository (..))
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository (..))
import RealWorld.Domain.Query.QueryService (QueryService (..))
import qualified RealWorld.Infra.Component.Database as Database
import qualified RealWorld.Infra.Component.HttpServer as HttpServerConfig
import qualified RealWorld.Infra.Interpreter.Adapter.Gateway.PasswordGateway as PasswordGatewayInterpreter
import qualified RealWorld.Infra.Interpreter.Adapter.Gateway.TokenGateway as TokenGatewayInterpreter
import qualified RealWorld.Infra.Interpreter.Adapter.Manager.TxManager as TxManagerInterpreter
import qualified RealWorld.Infra.Interpreter.Adapter.Repository.ArticleRepository as ArticleRepositoryInterpreter
import qualified RealWorld.Infra.Interpreter.Adapter.Repository.CommentRepository as CommentRepositoryInterpreter
import qualified RealWorld.Infra.Interpreter.Adapter.Repository.FavoriteRepository as FavoriteRepositoryInterpreter
import qualified RealWorld.Infra.Interpreter.Adapter.Repository.QueryService as QueryServiceInterpreter
import qualified RealWorld.Infra.Interpreter.Adapter.Repository.UserRepository as UserRepositoryInterpreter
import RealWorld.Infra.System (Config (logEnv), JwtSecret)
import qualified RealWorld.Infra.System as System
import RealWorld.Infra.Web.Auth (authTokenHandler)
import RealWorld.Infra.Web.Routes (Root, rootServer)
import Relude hiding (Reader, runReader)
import Servant (
  Context (EmptyContext, (:.)),
  Handler (..),
  serveWithContext,
 )
import Servant.Server (ServerError)

type App =
  Eff
    '[ QueryService
     , UserRepository
     , ArticleRepository
     , CommentRepository
     , FavoriteRepository
     , TokenGateway
     , PasswordGateway
     , TxManager
     , Reader JwtSecret
     , Reader Database.State
     , Error ServerError
     , KatipE
     , IOE
     ]

mainWithConfig :: System.Config -> IO ()
mainWithConfig config = do
  handleScribe <- mkHandleScribeWithFormatter jsonFormat ColorIfTerminal stdout (permitItem InfoS) V2
  let port = config & System.httpServer & HttpServerConfig.configPort
  let mkLogEnv =
        registerScribe "stdout" handleScribe defaultScribeSettings
          =<< initLogEnv "RealWorld" config.logEnv
  System.withState config $ \state' -> do
    bracket mkLogEnv closeScribes $ \le -> do
      Warp.run port (serveWithContext (Proxy :: Proxy Root) context $ rootServer $ handlerRunner le state')
 where
  context = authTokenHandler config.jwtSecret :. EmptyContext

runner :: LogEnv -> System.State -> App a -> IO (Either ServerError a)
runner le (database, jwtSecret) = do
  runEff -- IOE
    . runKatipContextE le () "main" -- KatipE
    . runErrorNoCallStack -- Error ServerError
    . runReader database -- Reader Database.State
    . runReader jwtSecret -- Reader JwtSecret
    . TxManagerInterpreter.run -- TxManager
    . PasswordGatewayInterpreter.run -- PasswordGateway
    . TokenGatewayInterpreter.run -- TokenGateway
    . FavoriteRepositoryInterpreter.run -- FavoriteRepository
    . CommentRepositoryInterpreter.run -- CommentRepository
    . ArticleRepositoryInterpreter.run -- ArticleRepository
    . UserRepositoryInterpreter.run -- UserRepository
    . QueryServiceInterpreter.run -- QueryService

handlerRunner :: LogEnv -> System.State -> App a -> Handler a
handlerRunner le state' = Handler . ExceptT . runner le state'

main :: IO ()
main = do
  config <- System.configFromEnv
  mainWithConfig config
