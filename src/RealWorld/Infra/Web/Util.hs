module RealWorld.Infra.Web.Util where

import qualified Data.Text as T
import RealWorld.Domain.User.Entity (Token (Token))
import RealWorld.Infra.Web.ErrorResponse
  ( ErrorResponse,
    raiseForbidden,
  )
import Relude
import Web.Scotty.Trans (ActionT, header)

withToken :: (MonadIO m) => (Token -> ActionT ErrorResponse m ()) -> ActionT ErrorResponse m ()
withToken action = do
  token <- getToken
  case token of
    Nothing -> raiseForbidden "token required"
    Just token' -> action token'

getToken :: (MonadIO m) => ActionT ErrorResponse m (Maybe Token)
getToken = do
  authorization <- header "Authorization"
  case authorization of
    Nothing -> pure Nothing
    Just authorization' -> case T.splitOn " " (toStrict authorization') of
      ["Token", token] -> pure $ Just $ Token token
      _ -> pure Nothing