module RealWorld.Util.Database where

import Control.Monad.Catch (MonadMask (mask), onException)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction
  ( TransactionMode,
    beginMode,
    commit,
    rollback,
  )
import Relude

withTransaction :: (MonadIO m, MonadMask m) => TransactionMode -> Connection -> m a -> m a
withTransaction mode conn act = mask $ \unmask -> do
  liftIO $ beginMode mode conn
  r <-
    unmask act `onException` do
      liftIO $ rollback conn
  liftIO $ commit conn
  pure r