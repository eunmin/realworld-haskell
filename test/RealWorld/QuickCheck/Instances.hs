{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.QuickCheck.Instances where

import Data.Text (pack)
import Data.ULID (ULID, ulidFromInteger)
import GHC.TypeLits (natVal)
import RealWorld.Domain.Command.Article.Entity.Article
  ( Article,
    mkArticle,
  )
import RealWorld.Domain.Command.Article.Value
  ( ArticleBody (ArticleBody),
    Description (Description),
    Slug (Slug),
    Tag (Tag),
    Title (Title),
  )
import RealWorld.Domain.Command.User.Entity.User (User (User))
import RealWorld.Domain.Command.User.Value
  ( Bio (Bio),
    Email (Email),
    HashedPassword (HashedPassword),
    Image (Image),
    Username (Username),
  )
import RealWorld.Domain.Util.BoundedText
  ( BoundedText (BoundedText),
  )
import Relude hiding (max, min, natVal)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    choose,
    chooseInt,
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

newtype ValidPasswordText = ValidPasswordText Text deriving (Eq, Show)

instance Arbitrary ValidPasswordText where
  arbitrary = do
    letter <- matchRegexp "[A-Za-z]{4,}"
    digit <- matchRegexp "[\\d]{4,}"
    special <- matchRegexp "[@$!%*#?&]{4,}"
    pure $ ValidPasswordText (letter <> digit <> special)

instance Arbitrary ULID where
  arbitrary = do
    let n :: Gen Integer = fromIntegral <$> chooseInt (0, maxBound :: Int)
    fromRight (error "Invalid ULID integer") . ulidFromInteger <$> n

instance Arbitrary Username where
  arbitrary = Username <$> arbitrary

instance Arbitrary Email where
  arbitrary = Email <$> arbitrary

instance Arbitrary HashedPassword where
  arbitrary = HashedPassword <$> arbitrary

instance Arbitrary Bio where
  arbitrary = Bio <$> arbitrary

instance Arbitrary Image where
  arbitrary = Image <$> arbitrary

instance Arbitrary User where
  arbitrary = do
    userId <- arbitrary
    username <- arbitrary
    email <- arbitrary
    hashedPassword <- arbitrary
    bio <- arbitrary
    image <- arbitrary
    createdAt <- arbitrary
    User userId username email hashedPassword bio image createdAt <$> arbitrary

instance Arbitrary Slug where
  arbitrary = Slug <$> arbitrary

instance Arbitrary Description where
  arbitrary = Description <$> arbitrary

instance Arbitrary Tag where
  arbitrary = Tag <$> arbitrary

instance Arbitrary Title where
  arbitrary = Title <$> matchRegexp "[A-Za-z\\s]{1,}"

instance Arbitrary ArticleBody where
  arbitrary = ArticleBody <$> arbitrary

instance Arbitrary Article where
  arbitrary = do
    articleId <- arbitrary
    title <- arbitrary
    description <- arbitrary
    body <- arbitrary
    tags <- arbitrary
    createdAt <- arbitrary
    mkArticle articleId title description body tags createdAt <$> arbitrary
