module Total
where

type Price = Double
type Label = String
type Code  = String
type Item = (Code,(Label,Price))

data ItemList = It Item ItemList
              | Nil

data Result = Result Price    
            | NoResult
    deriving (Show, Eq)

cash q p = fromInteger q * p

findPrice (It (code,item) items) s | code == s = Result $ snd item
                                   | otherwise = findPrice items s 
findPrice Nil _ = NoResult
