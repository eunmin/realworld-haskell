module RealWorld.Infra.System where

import Katip (Environment (..))
import RealWorld.Infra.Component.Database qualified as Database
import RealWorld.Infra.Component.HttpServer qualified as HttpServer
import Relude hiding (State, state, withState)
import System.Environment (getEnv)
import Prelude hiding (State, state, withState)

data Config = Config
  { httpServer :: HttpServer.Config
  , database :: Database.Config
  , jwtSecret :: Text
  , logEnv :: Environment
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
