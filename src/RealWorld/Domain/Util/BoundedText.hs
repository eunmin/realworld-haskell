{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RealWorld.Domain.Util.BoundedText where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import GHC.TypeLits (natVal)
import Relude hiding (natVal)

newtype BoundedText min max = BoundedText {unBoundedText :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

deriving newtype instance ToJSON (BoundedText min max)

mkBoundedText ::
  forall min max.
  (KnownNat min, KnownNat max) =>
  Text ->
  Maybe (BoundedText min max)
mkBoundedText t =
  if len > natVal (Proxy :: Proxy max) || len < natVal (Proxy :: Proxy min)
    then Nothing
    else Just $ BoundedText t
 where
  len = toInteger $ T.length t
