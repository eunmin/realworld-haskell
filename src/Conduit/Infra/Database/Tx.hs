{-# LANGUAGE FlexibleContexts #-}

module Conduit.Infra.Database.Tx where

import qualified Conduit.Infra.Component.Database as Database
import qualified Conduit.Infra.System as System
import Conduit.Util.Database (withTransaction)
import Conduit.Util.Pool (withResource)
import Control.Monad.Catch (MonadMask)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (..),
    ReadWriteMode (..),
    TransactionMode (..),
  )
import Relude

withTxMode :: (MonadState System.State m, MonadIO m, MonadMask m) => TransactionMode -> m a -> m a
withTxMode txMode action = do
  (Database.State {..}, secret) <- get
  withResource stateConnectionPool $ \conn -> do
    withTransaction txMode conn $ do
      modify (const (Database.State stateConnectionPool (Just conn), secret))
      action

defaultTransactionMode :: TransactionMode
defaultTransactionMode = TransactionMode ReadCommitted ReadWrite

withTx :: (MonadState System.State m, MonadIO m, MonadMask m) => ExceptT e m a -> ExceptT e m a
withTx = withTxMode defaultTransactionMode