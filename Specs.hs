import Test.Hspec
import Total
import Data.Char

main = hspec $ do
    let items = It ("AP",("apple",1.20)) (It ("BA",("banana",1.89)) Nil)

    describe "findItem" $ do
        it "should find the item or no result" $ do
            findItem items "AP" `shouldBe` Just ("apple",1.20)
            findItem items "BA" `shouldBe` Just ("banana",1.89)
            findItem items "PE" `shouldBe` Nothing 

    describe "total" $ do
        it "should return total price for a quantity and an item" $ do
            total items 10 "BA" `shouldBe` Just 18.90
        it "should not fail or give 0 when item not found" $ do
          total items 10 "PE" `shouldBe` Nothing 

    describe "itemMap" $ do
        it "should apply a function to a list of items" $ do
            let items' = itemMap (\(c,(l,p)) -> (c,(map toUpper l,p))) items
            items' `shouldBe` 
              It ("AP",("APPLE",1.20)) (It ("BA",("BANANA",1.89)) Nil)
            let items' = itemMap (\(c,(l,p)) -> (c,(l,p+0.1))) items
            items' `shouldBe` 
              It ("AP",("apple",1.30)) (It ("BA",("banana",1.99)) Nil)

    describe "fmap" $ do
        it "should apply a function to a function" $ do
            let f = const 41
                f'= fmap (+1) f
            f' 0 `shouldBe` 42 
            let p = (\(c,(l,p)) -> (c,(l,p+0.1)))
                u = (\(c,(l,p)) -> (c,(map toUpper l,p)))
                f = fmap p $ fmap u $ id
            f ("PE",("peach",3.0)) `shouldBe` ("PE",("PEACH",3.1))
