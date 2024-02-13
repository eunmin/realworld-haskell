{-# LANGUAGE DeriveAnyClass #-}

module Conduit.Infra.Web.Error where

import Data.Aeson (ToJSON)
import Network.HTTP.Types (Status)
import Relude
import Web.Scotty.Trans (ActionT, json, status)

data Error = Error
  {message :: Text}
  deriving (Show, Generic, ToJSON)

errorResponse :: (Monad m) => Status -> Text -> ActionT LText m ()
errorResponse httpStatus message = do
  status httpStatus
  json $ Error message