{-# LANGUAGE DataKinds #-}

module RealWorld.Domain.Command.User.ValueSpec where

import RealWorld.Domain.Command.User.Value
  ( Email (unEmail),
    Password (unPassword),
    Username (unUsername),
    mkEmail,
    mkPassword,
    mkUsername,
  )
import RealWorld.Domain.Util.BoundedText (BoundedText (..))
import RealWorld.QuickCheck.Instances
  ( InvalidEmailText (InvalidEmailText),
    ValidEmailText (ValidEmailText),
    ValidPasswordText (ValidPasswordText),
  )
import Relude
import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "mkUsername" $ do
    prop "with valid username"
      $ \(BoundedText x :: BoundedText 3 128) ->
        unBoundedText <$> (unUsername <$> mkUsername x) `shouldBe` Just x

    prop "with invalid username"
      $ \(BoundedText x :: BoundedText 1 2) ->
        unBoundedText <$> (unUsername <$> mkUsername x) `shouldBe` Nothing

  describe "mkEmail" $ do
    prop "with valid email"
      $ \(ValidEmailText x) -> unEmail <$> mkEmail x `shouldBe` Just x

    prop "with invalid email"
      $ \(InvalidEmailText x) -> unEmail <$> mkEmail x `shouldBe` Nothing

  describe "mkPassword" $ do
    prop "with valid password"
      $ \(ValidPasswordText x) -> unPassword <$> mkPassword x `shouldBe` Just x
