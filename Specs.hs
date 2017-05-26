import Test.Hspec
import Total
import Data.Char

main = hspec $ do
    let items = [("AP",("apple",1.20)), ("BA",("banana",1.89))]

    describe "total" $ do
        it "should return total price for a quantity and an item" $ do
            total items 10 "BA" `shouldBe` Just 18.90
        it "should not fail or give 0 when item not found" $ do
          total items 10 "PE" `shouldBe` Nothing 
