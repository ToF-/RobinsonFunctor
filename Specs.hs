import Test.Hspec
import Total

shouldBe_ a b = shouldBe (rounded a) (rounded b)
    where
    rounded x = ((fromInteger . truncate) (x * 10000)) / 10000 

main = hspec $ do
    describe "total" $ do
        it "should multiply a quantity by a price" $ do
            total 3 1.20 `shouldBe_` 3.60

