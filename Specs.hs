import Test.Hspec
import Total

shouldBe_ a b = shouldBe (rounded a) (rounded b)
    where
    rounded x = ((fromInteger . truncate) (x * 10000)) / 10000 

t = Item ("apple", 1.20) (Item ("banana", 1.34) Nil)

main = hspec $ do
    describe "total" $ do
        it "should multiply a quantity by a price" $ do
            total 3 1.20 `shouldBe_` 3.60

    describe "showTotal" $ do
        it "should display properly the total" $ do
            showTotal 3.00 `shouldBe` "3.00"
            showTotal 1.00 `shouldBe` "1.00"
            showTotal 1.24 `shouldBe` "1.24"
            showTotal 3.02 `shouldBe` "3.02"
            showTotal 3.02243 `shouldBe` "3.02"
            showTotal 3.2     `shouldBe` "3.20"
            showTotal 3.09143 `shouldBe` "3.09"
            showTotal 3.09543 `shouldBe` "3.10" 
            showTotal 3095.314 `shouldBe` "3095.31" 

    describe "findPrice" $ do
        it "should find the price of an item" $ do
            findPrice t "apple" `shouldBe` 1.20
            findPrice t "banana" `shouldBe` 1.34
            findPrice t "peach" `shouldBe` 0
            
    describe "cash" $ do 
        it "given a quantity and a ref, should give a total price" $ do
            cash t "20 banana"  `shouldBe` "26.80"
            cash t "10 apple"   `shouldBe` "12.00"

