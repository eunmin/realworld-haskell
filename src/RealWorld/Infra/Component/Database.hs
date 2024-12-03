{-# LANGUAGE FlexibleContexts #-}

module RealWorld.Infra.Component.Database where

import Control.Exception (bracket)
import Control.Exception.Safe (throwString)
import Control.Monad.Catch (MonadMask)
import Data.Has (Has (..))
import Data.Pool
import Database.PostgreSQL.Simple (
  ConnectInfo (
    ConnectInfo,
    connectDatabase,
    connectHost,
    connectPassword,
    connectPort,
    connectUser
  ),
  Connection,
  close,
  connect,
 )
import Database.PostgreSQL.Simple.Migration (
  MigrationCommand (MigrationDirectory, MigrationInitialization),
  MigrationResult (..),
  defaultOptions,
  runMigrations,
 )
import RealWorld.Infra.Util.Env (envRead)
import RealWorld.Infra.Util.Pool qualified as Pool
import Relude hiding (State, state, withState)
import System.Environment (getEnv)
import Prelude hiding (State, state, withState)

data Config = Config
  { configHost :: Text
  , configPort :: Int
  , configUser :: Text
  , configPassword :: Text
  , configDatabase :: Text
  , configPoolMaxSize :: Int
  , configPoolIdleTimeoutSec :: Double
  }
  deriving stock (Show, Eq)

data State = State
  { stateConnectionPool :: Pool Connection
  , stateConnection :: Maybe Connection
  }

withPool :: Config -> (Pool Connection -> IO a) -> IO a
withPool
  Config
    { configHost = host
    , configPort = port
    , configUser = user
    , configPassword = password
    , configDatabase = database
    , configPoolMaxSize = poolSize
    , configPoolIdleTimeoutSec = idleTimeoutSec
    } =
    bracket initPool cleanPool
    where
      initPool = newPool $ defaultPoolConfig openConn closeConn idleTimeoutSec poolSize
      cleanPool = destroyAllResources
      openConn =
        connect $
          ConnectInfo
            { connectHost = toString host
            , connectPort = fromIntegral port
            , connectUser = toString user
            , connectPassword = toString password
            , connectDatabase = toString database
            }
      closeConn = close

withState :: Config -> (State -> IO ()) -> IO ()
withState config action =
  withPool config $ \pool -> do
    let state = State pool Nothing
    migrate state
    action state

configFromEnv :: IO Config
configFromEnv =
  Config
    <$> (getEnv "DB_HOST" <&> toText)
    <*> envRead "DB_PORT"
    <*> (getEnv "DB_USER" <&> toText)
    <*> (getEnv "DB_PASSWORD" <&> toText)
    <*> (getEnv "DB_DATABASE" <&> toText)
    <*> envRead "DB_POOL_MAX_SIZE"
    <*> envRead "DB_POOL_IDLE_TIMEOUT_SEC"

migrate :: State -> IO ()
migrate (State pool _) = do
  withResource pool $ \conn -> do
    result <- runMigrations conn defaultOptions cmds
    case result of
      MigrationError err -> throwString err
      _ -> pass
  where
    cmds =
      [ MigrationInitialization
      , MigrationDirectory "sql/migrations"
      ]

withConnection ::
  (Has State r, MonadIO m, MonadReader r m, MonadMask m) =>
  (Connection -> m a) ->
  m a
withConnection action = do
  (State pool conn) <- asks getter
  case conn of
    Nothing -> Pool.withResource pool $ \conn' -> action conn'
    Just conn' -> action conn'
