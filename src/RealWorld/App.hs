{-# LANGUAGE UndecidableInstances #-}

module RealWorld.App (
  main,
)
where

import Control.Exception (bracket)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Katip (
  ColorStrategy (ColorIfTerminal),
  Katip,
  KatipContext,
  KatipContextT,
  Severity (InfoS),
  Verbosity (V2),
  closeScribes,
  defaultScribeSettings,
  initLogEnv,
  jsonFormat,
  mkHandleScribeWithFormatter,
  permitItem,
  registerScribe,
  runKatipContextT,
 )
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
import RealWorld.Infra.Component.HttpServer qualified as HttpServerConfig
import RealWorld.Infra.Database.PgArticleRepository qualified as PgArticleRepository
import RealWorld.Infra.Database.PgCommentRepository qualified as PgCommentRepository
import RealWorld.Infra.Database.PgFavoriteRepository qualified as PgFavoriteRepository
import RealWorld.Infra.Database.PgQueryService qualified as PgQueryService
import RealWorld.Infra.Database.PgUserRepository qualified as PgUserRepository
import RealWorld.Infra.Gateway.BcryptPasswordGateway qualified as BcryptPasswordGateway
import RealWorld.Infra.Gateway.JwtTokenGateway qualified as JwtTokenGateway
import RealWorld.Infra.Manager.PgTxManager qualified as PgTxManager
import RealWorld.Infra.System (Config (configLogEnv))
import RealWorld.Infra.System qualified as System
import RealWorld.Infra.Web.Routes (routes)
import Web.Scotty.Trans (scottyT)

newtype App a = App {unApp :: StateT System.State (KatipContextT IO) a}
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadState System.State
    , MonadMask
    , MonadFail
    , KatipContext
    , Katip
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
  findBySlug = PgArticleRepository.findBySlug
  delete = PgArticleRepository.delete

instance CommentRepository App where
  save = PgCommentRepository.save
  findById = PgCommentRepository.findById
  delete = PgCommentRepository.delete

instance FavoriteRepository App where
  findById = PgFavoriteRepository.findById
  save = PgFavoriteRepository.save
  delete = PgFavoriteRepository.delete

instance TokenGateway App where
  generate = JwtTokenGateway.generate
  verify = JwtTokenGateway.verify

instance PasswordGateway App where
  hashPassword = BcryptPasswordGateway.hashPassword
  isValidPassword = BcryptPasswordGateway.isValidPassword

instance TxManager App where
  withTx = PgTxManager.withTx

instance QueryService App where
  getCurrentUser = PgQueryService.getCurrentUser
  getProfile = PgQueryService.getProfile
  listArticles = PgQueryService.listArticles
  feedArticles = PgQueryService.feedArticles
  getArticle = PgQueryService.getArticle
  getComments = PgQueryService.getComments
  getTags = PgQueryService.getTags

mainWithConfig :: System.Config -> IO ()
mainWithConfig config = do
  handleScribe <- mkHandleScribeWithFormatter jsonFormat ColorIfTerminal stdout (permitItem InfoS) V2
  let port = config & System.configHttpServer & HttpServerConfig.configPort
  let mkLogEnv =
        registerScribe "stdout" handleScribe defaultScribeSettings
          =<< initLogEnv "RealWorld" (configLogEnv config)
  System.withState config $ \state' -> do
    bracket mkLogEnv closeScribes $ \le -> do
      scottyT
        port
        (\app -> fst <$> runKatipContextT le () "main" (runStateT (unApp app) state'))
        routes

main :: IO ()
main = do
  config <- System.configFromEnv
  mainWithConfig config
