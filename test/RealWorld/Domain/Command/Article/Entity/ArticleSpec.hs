module RealWorld.Domain.Command.Article.Entity.ArticleSpec where

import Data.ULID (ULID)
import RealWorld.Domain.Command.Article.Entity.Article (
  Article (articleAuthorId, articleFavoritesCount),
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
        isDeletable article (articleAuthorId article)
          `shouldBe` True

    prop "other user can't delete"
      $ \(article :: Article) (otherUserId :: ULID) -> do
        isDeletable article otherUserId
          `shouldBe` False

  describe "increseFavoritesCount" $ do
    prop "dobule increse"
      $ \(article :: Article) -> do
        articleFavoritesCount ((increseFavoritesCount . increseFavoritesCount) article)
          `shouldBe` articleFavoritesCount article + 2

  describe "increseFavoritesCount" $ do
    prop "dobule decrese"
      $ \(article :: Article) -> do
        articleFavoritesCount ((decreseFavoritesCount . decreseFavoritesCount) article)
          `shouldBe` articleFavoritesCount article - 2
