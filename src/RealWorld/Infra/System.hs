module RealWorld.Infra.System where

import qualified RealWorld.Infra.Component.Database as Database
import qualified RealWorld.Infra.Component.HttpServer as HttpServer
import RealWorld.Util.Env (envRead)
import Relude hiding (State, state, withState)

data Config = Config
  { configHttpServer :: HttpServer.Config,
    configDatabase :: Database.Config,
    configJwtSecret :: Text
  }
  deriving (Show, Eq)

type State = (Database.State, Text)

withState :: Config -> (State -> IO ()) -> IO ()
withState config action = do
  Database.withState (configDatabase config) $ \databaseState -> do
    action (databaseState, configJwtSecret config)

configFromEnv :: IO Config
configFromEnv =
  Config
    <$> HttpServer.configFromEnv
    <*> Database.configFromEnv
    <*> envRead "JWT_SECRET"

devConfig :: Config
devConfig =
  Config
    { configHttpServer =
        HttpServer.Config
          { HttpServer.configPort = 3000
          },
      configDatabase =
        Database.Config
          { Database.configHost = "localhost",
            Database.configPort = 5432,
            Database.configUser = "realworld",
            Database.configPassword = "",
            Database.configDatabase = "realworld_hs",
            Database.configPoolMaxSize = 12,
            Database.configPoolIdleTimeoutSec = 3.0
          },
      configJwtSecret = "secret"
    }