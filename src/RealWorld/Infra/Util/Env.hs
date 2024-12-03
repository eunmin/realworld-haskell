module RealWorld.Infra.Util.Env where

import Control.Exception.Safe (throwString)
import Safe (readMay)
import System.Environment (getEnv)

envRead :: (Read a) => String -> IO a
envRead key = do
  rawVal <- getEnv key
  case readMay rawVal of
    Just val -> pure val
    Nothing -> throwString $ key <> ": Unable to parse " <> rawVal