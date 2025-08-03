{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module RealWorld.Infra.Web.ErrorResponse where

import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as JSON
import Network.HTTP.Types (Status)
import Network.HTTP.Types.Status (
  status401,
  status403,
  status404,
  status422,
 )
import Relude
import Servant (
  ServerError (errBody, errHeaders),
  err400,
  err401,
  err404,
  err503,
 )

data ErrorResponse = ErrorResponse
  { status :: Status
  , errors :: Errors
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

data Except = Except
  { status :: Status
  , message :: Text
  }
  deriving stock (Show, Eq, Typeable)
  deriving anyclass (Exception)

invalid :: Text -> Except
invalid = Except status422

unauthorized :: Text -> Except
unauthorized = Except status401

forbidden :: Text -> Except
forbidden = Except status403

notFound :: Text -> Except
notFound = Except status404

-- handleEx :: (MonadIO m) => ErrorHandler m
-- handleEx = Handler $ \(Except status' message) -> do
--   Scotty.status status'
--   json $ Errors $ ErrorsBody [message]

jsonError :: ServerError -> Text -> ServerError
jsonError serverError message =
  serverError
    { errBody = JSON.encode body
    , errHeaders = [("Content-Type", "application/json")]
    }
 where
  body = Errors $ ErrorsBody [message]

unauthorized' :: Text -> ServerError
unauthorized' = jsonError err401

notFound' :: Text -> ServerError
notFound' = jsonError err404

badRequest :: Text -> ServerError
badRequest = jsonError err400

unavailable :: Text -> ServerError
unavailable = jsonError err503
