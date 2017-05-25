module Total
where

type Quantity = Integer
type Price    = Double
type Ref      = String

total :: Quantity -> Result -> Result
total q p = case p of
    Result r -> Result $ r * fromInteger q 
    NoResult -> NoResult

data Items = Nil
           | Item (Ref, Price) Items

data Result = NoResult
            | Result Double 
    deriving (Eq, Show)

findPrice :: Items -> Ref -> Result
findPrice (Nil) _ = NoResult 
findPrice (Item (r,p) is) s | r == s = Result p
                            | otherwise = findPrice is s 

showTotal :: Result -> String
showTotal (Result n) = 
    let m = show $ truncate $ (n + 0.005) * 100 
        l = length m - 2
    in (take l m) ++ "." ++ (drop l m) 
showTotal NoResult = "??"

cash :: Items -> String -> String
cash t s = let [q,r] = words s in showTotal $ total (read q) (findPrice t r)


