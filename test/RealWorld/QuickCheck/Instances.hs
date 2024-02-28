module RealWorld.QuickCheck.Instances where

import Data.Text (pack)
import Relude hiding (max, min)
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

newtype ValidBoundedText = ValidBoundedText Text deriving (Eq, Show)

instance Arbitrary ValidBoundedText where
  arbitrary = ValidBoundedText <$> genTextBounded 1 10

newtype InvalidBoundedText = InvalidBoundedText Text deriving (Eq, Show)

instance Arbitrary InvalidBoundedText where
  arbitrary = InvalidBoundedText <$> genTextBounded 11 20

newtype ValidEmailText = ValidEmailText Text deriving (Eq, Show)

instance Arbitrary ValidEmailText where
  arbitrary = ValidEmailText <$> matchRegexp "[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}"

newtype InvalidEmailText = InvalidEmailText Text deriving (Eq, Show)

instance Arbitrary InvalidEmailText where
  arbitrary = InvalidEmailText <$> arbitrary
