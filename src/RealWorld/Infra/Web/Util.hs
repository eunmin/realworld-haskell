module RealWorld.Infra.Web.Util where

import qualified Data.Text as T
import RealWorld.Infra.Web.ErrorResponse
  ( forbidden,
  )
import Relude
import Web.Scotty.Trans (ActionT, header, throw)

withRequiredToken :: (MonadIO m) => (Text -> ActionT m ()) -> ActionT m ()
withRequiredToken action = do
  token <- getToken
  case token of
    Nothing -> throw $ forbidden "token required"
    Just token' -> action token'

withOptionalToken :: (MonadIO m) => (Maybe Text -> ActionT m ()) -> ActionT m ()
withOptionalToken action = do
  token <- getToken
  action token

getToken :: (MonadIO m) => ActionT m (Maybe Text)
getToken = do
  authorization <- header "Authorization"
  case authorization of
    Nothing -> pure Nothing
    Just authorization' -> case T.splitOn " " (toStrict authorization') of
      ["Token", token] -> pure $ Just token
      _ -> pure Nothing

(!?) :: (MonadIO m, Exception e) => m (Maybe a) -> e -> ActionT m a
(!?) value e = do
  value' <- lift value
  case value' of
    Just a -> pure a
    Nothing -> throw e
