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


total items q s = mapf ((fromInteger q)*) $ mapf snd $ findItem items s

findItem (It (code,item) items) s | code == s = Result item
                                  | otherwise = findItem items s 
findItem Nil _ = NoResult

    
mapf f (Result x) = Result $ f x
mapf f NoResult   = NoResult
