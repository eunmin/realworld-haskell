{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module RealWorld.Infra.Web.ErrorResponse where

import Control.Error (headMay)
import Data.Aeson (ToJSON (..))
import Network.HTTP.Types (Status)
import Network.HTTP.Types.Status (
  status401,
  status403,
  status404,
  status422,
  status500,
 )
import Web.Scotty.Internal.Types (ScottyError (..))

data ErrorResponse = ErrorResponse
  { errroResponseStatus :: Status
  , errorResponseErrors :: Errors
  }
  deriving stock (Show, Generic)

data Errors = Errors
  {errors :: ErrorsBody}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
data ErrorsBody = ErrorsBody
  {body :: [Text]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance ScottyError ErrorResponse where
  stringError = mkErrorResponse status500 . toText
  showError = toLazy . fromMaybe "Unknown Error" . headMay . body . errors . errorResponseErrors

mkErrorResponse :: Status -> Text -> ErrorResponse
mkErrorResponse status message = ErrorResponse status $ Errors $ ErrorsBody [message]

invalid :: Text -> ErrorResponse
invalid = mkErrorResponse status422

unauthorized :: Text -> ErrorResponse
unauthorized = mkErrorResponse status401

forbidden :: Text -> ErrorResponse
forbidden = mkErrorResponse status403

notFound :: Text -> ErrorResponse
notFound = mkErrorResponse status404