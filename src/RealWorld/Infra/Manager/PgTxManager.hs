{-# LANGUAGE FlexibleContexts #-}

module RealWorld.Infra.Manager.PgTxManager where

import Control.Monad.Catch (MonadMask (mask), onException)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (..),
    ReadWriteMode (..),
    TransactionMode (..),
    beginMode,
    commit,
    rollback,
  )
import qualified RealWorld.Infra.Component.Database as Database
import qualified RealWorld.Infra.System as System
import RealWorld.Infra.Util.Pool (withResource)
import Relude

defaultTransactionMode :: TransactionMode
defaultTransactionMode = TransactionMode ReadCommitted ReadWrite

withTransaction :: (MonadIO m, MonadMask m) => TransactionMode -> Connection -> m a -> m a
withTransaction mode conn act = mask $ \unmask -> do
  liftIO $ beginMode mode conn
  r <-
    unmask act `onException` do
      liftIO $ rollback conn
  liftIO $ commit conn
  pure r

withTxMode :: (MonadReader System.State m, MonadIO m, MonadMask m) => TransactionMode -> m a -> m a
withTxMode txMode action = do
  (Database.State {..}, secret) <- ask
  withResource stateConnectionPool $ \conn -> do
    withTransaction txMode conn $ do
      local (const (Database.State stateConnectionPool (Just conn), secret)) $ do
        action

withTx :: (MonadReader System.State m, MonadIO m, MonadMask m) => ExceptT e m a -> ExceptT e m a
withTx = withTxMode defaultTransactionMode
