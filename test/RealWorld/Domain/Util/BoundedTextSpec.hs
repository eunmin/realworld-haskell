{-# LANGUAGE DataKinds #-}

module RealWorld.Domain.Util.BoundedTextSpec where

import RealWorld.Domain.Util.BoundedText
  ( BoundedText (..),
    mkBoundedText,
  )
import RealWorld.QuickCheck.Instances ()
import Relude
import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "mkBoundedText :: BoundedText 1 10" $ do
    prop "with valid text"
      $ \(BoundedText x :: BoundedText 1 10) ->
        unBoundedText <$> (mkBoundedText x :: Maybe (BoundedText 1 10)) `shouldBe` Just x
    prop "with invalid text"
      $ \(BoundedText x :: BoundedText 11 20) ->
        unBoundedText <$> (mkBoundedText x :: Maybe (BoundedText 1 10)) `shouldBe` Nothing