module RealWorld.Infra.Component.HttpServer where

import RealWorld.Infra.Util.Env (envRead)
import Relude

data Config = Config
  { configPort :: Int
  }
  deriving (Show, Eq)

configFromEnv :: IO Config
configFromEnv =
  Config <$> envRead "PORT"