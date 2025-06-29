module RealWorld.Domain.Command.Article.Entity.ArticleSpec where

import Data.ULID (ULID)
import RealWorld.Domain.Command.Article.Entity.Article
  ( Article (..),
    decreseFavoritesCount,
    increseFavoritesCount,
    isDeletable,
  )
import RealWorld.QuickCheck.Instances ()
import Relude
import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "isDeletable" $ do
    prop "author can delete"
      $ \(article :: Article) -> do
        isDeletable article article.authorId
          `shouldBe` True

    prop "other user can't delete"
      $ \(article :: Article) (otherUserId :: ULID) -> do
        isDeletable article otherUserId
          `shouldBe` False

  describe "increseFavoritesCount" $ do
    prop "dobule increse"
      $ \(article :: Article) -> do
        let article' = (increseFavoritesCount . increseFavoritesCount) article
        article'.favoritesCount
          `shouldBe` article.favoritesCount + 2

  describe "increseFavoritesCount" $ do
    prop "dobule decrese"
      $ \(article :: Article) -> do
        let article' = (decreseFavoritesCount . decreseFavoritesCount) article
        article'.favoritesCount
          `shouldBe` article.favoritesCount - 2
