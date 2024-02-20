module RealWorld.Infra.Web.Util where

import qualified Data.Text as T
import RealWorld.Domain.Command.User.Value (Token (Token))
import RealWorld.Infra.Web.ErrorResponse
  ( ErrorResponse,
    forbidden,
  )
import Relude
import Web.Scotty.Trans (ActionT, ScottyError, header, raise)

withToken :: (MonadIO m) => (Token -> ActionT ErrorResponse m ()) -> ActionT ErrorResponse m ()
withToken action = do
  token <- getToken
  case token of
    Nothing -> raise $ forbidden "token required"
    Just token' -> action token'

getToken :: (MonadIO m) => ActionT ErrorResponse m (Maybe Token)
getToken = do
  authorization <- header "Authorization"
  case authorization of
    Nothing -> pure Nothing
    Just authorization' -> case T.splitOn " " (toStrict authorization') of
      ["Token", token] -> pure $ Just $ Token token
      _ -> pure Nothing

(!?) :: (Monad m, ScottyError e) => m (Maybe a) -> e -> ActionT e m a
(!?) value e = do
  value' <- lift value
  case value' of
    Just a -> pure a
    Nothing -> raise e