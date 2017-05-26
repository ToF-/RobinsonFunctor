module Total
where

type Price = Double
type Label = String
type Code  = String
type Item = (Code,(Label,Price))

data ItemList = It Item ItemList
              | Nil

data Result a = Result a 
            | NoResult
    deriving (Show, Eq)

cash q p = fromInteger q * p

findPrice items s = mapf snd $ findItem items s

total items q s = mapf (cash q) $ findPrice items s

findItem (It (code,item) items) s | code == s = Result item
                                  | otherwise = findItem items s 
findItem Nil _ = NoResult

    
mapf f (Result x) = Result $ f x
mapf f NoResult   = NoResult
