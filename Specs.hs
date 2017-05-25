import Test.Hspec
import Total

main = hspec $ do
    describe "cash" $ do
        it "should multiply a quantity by a price" $ do
            cash 5 1.20 `shouldBe` 6.00
            cash 2 3.42 `shouldBe` 6.84

    describe "findPrice" $ do
        let items = It ("AP",("apple",1.20)) (It ("BA",("banana",1.89)) Nil)
        it "should find the price of an item in a list of items" $ do
            findPrice items "AP" `shouldBe` Result 1.20
            findPrice items "BA" `shouldBe` Result 1.89
        it "should not fail or give 0 when item not found" $ do
            findPrice items "PE" `shouldBe` NoResult 
