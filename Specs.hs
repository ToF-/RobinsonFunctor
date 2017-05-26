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

    describe "readQuantity" $ do
        it "should read a quantity from a String" $ do
            readQuantity "42" `shouldBe` Just 42
            readQuantity "17" `shouldBe` Just 17
        it "should not fail or give 0 when quantity not parsed" $ do
            readQuantity "foo" `shouldBe` Nothing
             
