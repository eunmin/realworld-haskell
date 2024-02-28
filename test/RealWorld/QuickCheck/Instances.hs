{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.QuickCheck.Instances where

import Data.Text (pack)
import GHC.TypeLits (natVal)
import RealWorld.Domain.Util.BoundedText
import Relude hiding (max, min, natVal)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    choose,
    listOf,
    suchThat,
  )
import Test.QuickCheck.Instances ()
import Test.QuickCheck.StringRandom (matchRegexp)

genTextBounded :: Int -> Int -> Gen Text
genTextBounded min max =
  pack
    <$> listOf (choose ('A', 'z'))
    `suchThat` (\x -> length x >= min && length x <= max)

newtype ValidEmailText = ValidEmailText Text deriving (Eq, Show)

instance Arbitrary ValidEmailText where
  arbitrary = ValidEmailText <$> matchRegexp "[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}"

newtype InvalidEmailText = InvalidEmailText Text deriving (Eq, Show)

instance Arbitrary InvalidEmailText where
  arbitrary = InvalidEmailText <$> arbitrary

instance
  forall min max.
  (KnownNat min, KnownNat max) =>
  Arbitrary (BoundedText min max)
  where
  arbitrary =
    BoundedText
      <$> genTextBounded
        (fromIntegral $ natVal (Proxy :: Proxy min))
        (fromIntegral $ natVal (Proxy :: Proxy max))
