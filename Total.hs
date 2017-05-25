module Total
where

total :: Integer -> Double -> Double
total q p = p * fromInteger q 

showTotal :: Double -> String
showTotal n = 
    let m = show $ truncate $ (n + 0.005) * 100 
        l = length m - 2
    in (take l m) ++ "." ++ (drop l m) 



