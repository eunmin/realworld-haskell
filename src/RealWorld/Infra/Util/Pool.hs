module RealWorld.Infra.Util.Pool where

import Control.Monad.Catch (MonadMask (mask), onException)
import Data.Pool (Pool, destroyResource, putResource, takeResource)
import Relude

withResource :: (MonadIO m, MonadMask m) => Pool a -> (a -> m r) -> m r
withResource pool act = mask $ \unmask -> do
  (res, localPool) <- liftIO $ takeResource pool
  r <-
    unmask (act res) `onException` do
      liftIO $ destroyResource pool localPool res
  liftIO $ putResource localPool res
  pure r
