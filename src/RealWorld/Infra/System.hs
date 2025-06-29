module RealWorld.Infra.System where

import Katip (Environment (..))
import qualified RealWorld.Infra.Component.Database as Database
import qualified RealWorld.Infra.Component.HttpServer as HttpServer
import Relude hiding (State, state, withState)
import System.Environment (getEnv)

data Config = Config
  { httpServer :: HttpServer.Config,
    database :: Database.Config,
    jwtSecret :: Text,
    logEnv :: Environment
  }
  deriving stock (Show, Eq)

type State = (Database.State, Text)

withState :: Config -> (State -> IO ()) -> IO ()
withState config action = do
  Database.withState config.database $ \databaseState -> do
    action (databaseState, config.jwtSecret)

configFromEnv :: IO Config
configFromEnv =
  Config
    <$> HttpServer.configFromEnv
    <*> Database.configFromEnv
    <*> (getEnv "JWT_SECRET" <&> toText)
    <*> (Environment . toText <$> getEnv "ENV_NAME")
