{-# LANGUAGE FlexibleContexts #-}

module Conduit.Infra.Database.Tx where

import qualified Conduit.Infra.Component.Database as Database
import qualified Conduit.Infra.System as System
import Control.Monad.Catch (MonadMask (mask), onException)
import Data.Pool (Pool, destroyResource, putResource, takeResource)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction
  ( TransactionMode,
    beginMode,
    commit,
    defaultTransactionMode,
    rollback,
  )
import Relude

withResource :: (MonadIO m, MonadMask m) => Pool a -> (a -> m r) -> m r
withResource pool act = mask $ \unmask -> do
  (res, localPool) <- liftIO $ takeResource pool
  r <-
    unmask (act res) `onException` do
      liftIO $ destroyResource pool localPool res
  liftIO $ putResource localPool res
  pure r

withTransaction :: (MonadIO m, MonadMask m) => TransactionMode -> Connection -> m a -> m a
withTransaction mode conn act = mask $ \unmask -> do
  liftIO $ beginMode mode conn
  r <-
    unmask act `onException` do
      liftIO $ rollback conn
  liftIO $ commit conn
  pure r

withTx :: (MonadState System.State m, MonadIO m, MonadMask m) => m a -> m a
withTx action = do
  (Database.State {..}, secret) <- get
  withResource stateConnectionPool $ \conn -> do
    withTransaction defaultTransactionMode conn $ do
      modify (const (Database.State stateConnectionPool (Just conn), secret))
      action