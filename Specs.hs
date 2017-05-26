import Test.Hspec
import Total

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
