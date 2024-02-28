{-# LANGUAGE DataKinds #-}

module RealWorld.Domain.Util.BoundedTextSpec where

import RealWorld.Domain.Util.BoundedText
  ( BoundedText (unBoundedText),
    mkBoundedText,
  )
import RealWorld.QuickCheck.Instances
  ( InvalidBoundedText (InvalidBoundedText),
    ValidBoundedText (ValidBoundedText),
  )
import Relude
import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "mkBoundedText :: BoundedText 1 10" $ do
    prop "with ValidBoundedText"
      $ \(ValidBoundedText x) ->
        unBoundedText <$> (mkBoundedText x :: Maybe (BoundedText 1 10)) `shouldBe` Just x
    prop "with InvalidBoundedText"
      $ \(InvalidBoundedText x) ->
        unBoundedText <$> (mkBoundedText x :: Maybe (BoundedText 1 10)) `shouldBe` Nothing