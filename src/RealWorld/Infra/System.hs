module RealWorld.Infra.System where

import Katip (Environment (..))
import RealWorld.Infra.Component.Database qualified as Database
import RealWorld.Infra.Component.HttpServer qualified as HttpServer
import Relude hiding (State, state, withState)
import System.Environment (getEnv)
import Prelude hiding (State, state, withState)

data Config = Config
  { configHttpServer :: HttpServer.Config
  , configDatabase :: Database.Config
  , configJwtSecret :: Text
  , configLogEnv :: Environment
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
    <*> (getEnv "JWT_SECRET" <&> toText)
    <*> (Environment . toText <$> getEnv "ENV_NAME")

devConfig :: Config
devConfig =
  Config
    { configHttpServer =
        HttpServer.Config
          { HttpServer.configPort = 3000
          }
    , configDatabase =
        Database.Config
          { Database.configHost = "localhost"
          , Database.configPort = 5432
          , Database.configUser = "realworld"
          , Database.configPassword = ""
          , Database.configDatabase = "realworld_hs"
          , Database.configPoolMaxSize = 12
          , Database.configPoolIdleTimeoutSec = 3.0
          }
    , configJwtSecret = "secret"
    , configLogEnv = "dev"
    }