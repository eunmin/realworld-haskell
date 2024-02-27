module RealWorld.Domain.Command.User.ValueSpec where

import Data.Text.Arbitrary ()
import RealWorld.Domain.Command.User.Value
import Relude
import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.StringRandom (matchRegexp)

newtype ValidEmailText = ValidEmailText Text deriving (Eq, Show)

instance Arbitrary ValidEmailText where
  arbitrary = ValidEmailText <$> matchRegexp "[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}"

newtype InvalidEmailText = InvalidEmailText Text deriving (Eq, Show)

instance Arbitrary InvalidEmailText where
  arbitrary = InvalidEmailText <$> arbitrary

spec :: Spec
spec = do
  describe "mkEmail" $ do
    prop "is valid email"
      $ \(ValidEmailText x) -> unEmail <$> mkEmail x `shouldBe` Just x
    prop "is invalid email"
      $ \(InvalidEmailText x) -> unEmail <$> mkEmail x `shouldBe` Nothing