{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RealWorld.Infra.Converter.Aeson where

import Data.Aeson (ToJSON (toJSON), withText)
import Data.Aeson.Types (FromJSON (..))
import Data.ULID (ULID)
import Relude

instance ToJSON ULID where
  toJSON = show

instance FromJSON ULID where
  parseJSON =
    withText "ULID" $
      maybe (fail "invalid ULID") pure
        . readMaybe
        . toString