import Test.Hspec
import Total
import Data.Char

main = hspec $ do
    let items = It ("AP",("apple",1.20)) (It ("BA",("banana",1.89)) Nil)

    describe "findItem" $ do
        it "should find the item or no result" $ do
            findItem items "AP" `shouldBe` Result ("apple",1.20)
            findItem items "BA" `shouldBe` Result ("banana",1.89)
            findItem items "PE" `shouldBe` NoResult 

    describe "total" $ do
        it "should return total price for a quantity and an item" $ do
            total items 10 "BA" `shouldBe` Result 18.90
        it "should not fail or give 0 when item not found" $ do
            total items 10 "PE" `shouldBe` NoResult 

    describe "itemMap" $ do
        it "should apply a function to a list of items" $ do
            let items' = itemMap (\(c,(l,p)) -> (c,(map toUpper l,p))) items
            items' `shouldBe` 
              It ("AP",("APPLE",1.20)) (It ("BA",("BANANA",1.89)) Nil)
            let items' = itemMap (\(c,(l,p)) -> (c,(l,p+0.1))) items
            items' `shouldBe` 
              It ("AP",("apple",1.30)) (It ("BA",("banana",1.99)) Nil)
