{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Converter.PostgreSQL where

import Data.ULID (ULID)
import Database.PostgreSQL.Simple.FromField
  ( FromField (fromField),
    ResultError (ConversionFailed, UnexpectedNull),
    returnError,
  )
import Database.PostgreSQL.Simple.ToField
  ( Action (Escape),
    ToField (..),
  )
import Relude

instance ToField ULID where
  toField = Escape . show

instance FromField ULID where
  fromField f mbs =
    case decodeUtf8 <$> mbs of
      Nothing -> returnError UnexpectedNull f ""
      Just dat ->
        case readMaybe dat of
          Nothing -> returnError ConversionFailed f dat
          Just x -> pure x
