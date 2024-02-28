module RealWorld.Domain.Command.User.ValueSpec where

import RealWorld.Domain.Command.User.Value
  ( Email (unEmail),
    mkEmail,
  )
import RealWorld.QuickCheck.Instances
  ( InvalidEmailText (InvalidEmailText),
    ValidEmailText (ValidEmailText),
  )
import Relude
import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "mkEmail" $ do
    prop "is valid email"
      $ \(ValidEmailText x) -> unEmail <$> mkEmail x `shouldBe` Just x
    prop "is invalid email"
      $ \(InvalidEmailText x) -> unEmail <$> mkEmail x `shouldBe` Nothing