module Total
where

total :: Integer -> Double -> Double
total q p = p * fromInteger q 

data Items = Nil
           | Item (String,Double) Items

findPrice (Nil) _ = 0
findPrice (Item (r,p) is) s | r == s = p
                            | otherwise = findPrice is s 

showTotal :: Double -> String
showTotal n = 
    let m = show $ truncate $ (n + 0.005) * 100 
        l = length m - 2
    in (take l m) ++ "." ++ (drop l m) 



