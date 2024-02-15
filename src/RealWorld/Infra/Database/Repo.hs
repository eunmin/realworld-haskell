{-# LANGUAGE FlexibleContexts #-}

module RealWorld.Infra.Database.Repo where

import Control.Monad.Catch (MonadMask)
import Data.Has (Has (..))
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction
  ( IsolationLevel (..),
    ReadWriteMode (..),
    TransactionMode (..),
  )
import qualified RealWorld.Infra.Component.Database as Database
import qualified RealWorld.Infra.System as System
import RealWorld.Util.Database (withTransaction)
import RealWorld.Util.Pool (withResource)
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

withConnection ::
  (Has Database.State r, MonadIO m, MonadState r m, MonadMask m) =>
  (Connection -> m a) ->
  m a
withConnection action = do
  (Database.State pool conn) <- gets getter
  case conn of
    Nothing -> withResource pool $ \conn' -> action conn'
    Just conn' -> action conn'