{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RealWorld.Domain.Util.BoundedText where

import Data.Text qualified as T
import GHC.TypeLits (natVal)
import Relude hiding (natVal)
import Prelude hiding (natVal)

newtype BoundedText min max = BoundedText {unBoundedText :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

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