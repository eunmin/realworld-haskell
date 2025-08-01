{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module RealWorld.Infra.Web.ErrorResponse where

import Data.Aeson (ToJSON (..))
import Network.HTTP.Types (Status)
import Network.HTTP.Types.Status
  ( status401,
    status403,
    status404,
    status422,
  )
import Relude
import Web.Scotty.Internal.Types (ErrorHandler)
import Web.Scotty.Trans (Handler (..), json)
import qualified Web.Scotty.Trans as Scotty

data ErrorResponse = ErrorResponse
  { status :: Status,
    errors :: Errors
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
  { status :: Status,
    message :: Text
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

handleEx :: (MonadIO m) => ErrorHandler m
handleEx = Handler $ \(Except status' message) -> do
  Scotty.status status'
  json $ Errors $ ErrorsBody [message]
