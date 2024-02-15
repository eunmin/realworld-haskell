{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Web.ErrorResponse where

import Control.Error (headMay)
import Data.Aeson (ToJSON (..))
import Network.HTTP.Types (Status)
import Network.HTTP.Types.Status
  ( status401,
    status403,
    status404,
    status422,
    status500,
  )
import Relude
import Web.Scotty.Internal.Types (ScottyError (..))
import Web.Scotty.Trans (ActionT, raise)

data ErrorResponse = ErrorResponse
  { errroResponseStatus :: Status,
    errorResponseErrors :: Errors
  }
  deriving (Show, Generic)

data Errors = Errors
  {errors :: ErrorsBody}
  deriving (Show, Generic, ToJSON)

data ErrorsBody = ErrorsBody
  {body :: [Text]}
  deriving (Show, Generic, ToJSON)

instance ScottyError ErrorResponse where
  stringError = mkErrorResponse status500 . toText
  showError = toLazy . fromMaybe "Unknown Error" . headMay . body . errors . errorResponseErrors

mkErrorResponse :: Status -> Text -> ErrorResponse
mkErrorResponse status message = ErrorResponse status $ Errors $ ErrorsBody [message]

raiseInvalidate :: (Monad m) => Text -> ActionT ErrorResponse m ()
raiseInvalidate message = raise $ mkErrorResponse status422 message

raiseUnauthorized :: (Monad m) => Text -> ActionT ErrorResponse m ()
raiseUnauthorized message = raise $ mkErrorResponse status401 message

raiseForbidden :: (Monad m) => Text -> ActionT ErrorResponse m ()
raiseForbidden message = raise $ mkErrorResponse status403 message

raiseNotFound :: (Monad m) => Text -> ActionT ErrorResponse m ()
raiseNotFound message = raise $ mkErrorResponse status404 message
