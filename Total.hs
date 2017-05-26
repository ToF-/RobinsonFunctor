module Total
where

type Price = Double
type Label = String
type Code  = String
type Item = (Code,(Label,Price))

data ItemList = It Item ItemList
              | Nil
    deriving (Show, Eq)

data Result a = Result a 
            | NoResult
    deriving (Show, Eq)


total items q s = resultMap ((fromInteger q)*) $ resultMap snd $ findItem items s

findItem (It (code,item) items) s | code == s = Result item
                                  | otherwise = findItem items s 
findItem Nil _ = NoResult

    
resultMap f (Result x) = Result $ f x
resultMap f NoResult   = NoResult

itemMap f (It item items) = It (f item) (itemMap f items)
itemMap _ Nil = Nil
