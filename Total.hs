module Total
where

type Price = Double
type Quantity = Integer
type Label = String
type Code  = String
type Item = (Code,(Label,Price))

data ItemList = It Item ItemList
              | Nil
    deriving (Show, Eq)

data Result a = Result a 
            | NoResult
    deriving (Show, Eq)


total :: ItemList -> Quantity -> Code -> Result Price
total items q s = resultMap ((fromInteger q)*) $ resultMap snd $ findItem items s

findItem :: ItemList -> Code -> Result (Label,Price)
findItem (It (code,item) items) s | code == s = Result item
                                  | otherwise = findItem items s 
findItem Nil _ = NoResult

    
resultMap :: (a -> b) -> Result a -> Result b
resultMap f (Result x) = Result $ f x
resultMap f NoResult   = NoResult

itemMap :: (Item -> Item) -> ItemList -> ItemList
itemMap f (It item items) = It (f item) (itemMap f items)
itemMap _ Nil = Nil

functionMap f g = f . g 
