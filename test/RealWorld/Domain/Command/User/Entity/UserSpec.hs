module RealWorld.Domain.Command.User.Entity.UserSpec where

import RealWorld.Domain.Command.User.Entity.User (User, update)
import RealWorld.Domain.Command.User.Value
  ( Bio,
    Email,
    HashedPassword,
    Image,
    Username,
  )
import RealWorld.QuickCheck.Instances ()
import Relude
import Test.Hspec (Spec, describe, shouldBe, shouldNotBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "update" $ do
    prop "should pass"
      $ \(user :: User)
         (username :: Username)
         (email :: Email)
         (hashedPassword :: HashedPassword)
         (bio :: Bio)
         (image :: (Maybe Image)) ->
          update user (Just username) (Just email) (Just hashedPassword) (Just bio) (Just image)
            `shouldNotBe` user

    prop "change nothing"
      $ \(user :: User) -> update user Nothing Nothing Nothing Nothing Nothing `shouldBe` user

    prop "idempotence"
      $ \(user :: User)
         (username :: Maybe Username)
         (email :: Maybe Email)
         (hashedPassword :: Maybe HashedPassword)
         (bio :: Maybe Bio)
         (image :: Maybe (Maybe Image)) ->
          do
            let updatedUser = update user username email hashedPassword bio image
            update user username email hashedPassword bio image `shouldBe` updatedUser
