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

findPrice items s = case findItem items s of
    Result item -> Result $ snd item
    NoResult    -> NoResult

findItem (It (code,item) items) s | code == s = Result item
                                  | otherwise = findItem items s 
findItem Nil _ = NoResult

total items q s = case findPrice items s of
    Result p -> Result $ cash q p
    NoResult -> NoResult
    
