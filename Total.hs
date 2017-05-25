module Total
where

type Quantity = Integer
type Price    = Double
type Ref      = String

total :: Quantity -> Price -> Price
total q p = p * fromInteger q 

data Items = Nil
           | Item (Ref, Price) Items

findPrice :: Items -> Ref -> Price
findPrice (Nil) _ = 0
findPrice (Item (r,p) is) s | r == s = p
                            | otherwise = findPrice is s 

showTotal :: Price -> String
showTotal n = 
    let m = show $ truncate $ (n + 0.005) * 100 
        l = length m - 2
    in (take l m) ++ "." ++ (drop l m) 



