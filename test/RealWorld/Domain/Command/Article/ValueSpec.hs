module RealWorld.Domain.Command.Article.ValueSpec where

import Data.Char (isUpper)
import RealWorld.Domain.Command.Article.Value (Slug (unSlug), Title (..), toSlug)
import RealWorld.QuickCheck.Instances ()
import Relude
import Test.Hspec
  ( Spec,
    describe,
    shouldBe,
    shouldNotContain,
    shouldSatisfy,
  )
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "toSlug" $ do
    prop "should not contain space"
      $ \(title :: Title) ->
        (toString . unSlug . toSlug) title `shouldNotContain` " "

    prop "should not contain upper case"
      $ \(title :: Title) -> do
        (toString . unSlug . toSlug) title `shouldSatisfy` \x -> do
          not (any isUpper x)

    prop "should same length"
      $ \(title :: Title) ->
        length (toString . unSlug . toSlug $ title) `shouldBe` length (toString . unTitle $ title)
