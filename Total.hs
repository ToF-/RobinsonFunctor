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


total :: ItemList -> Quantity -> Code -> Maybe Price
total items q s = fmap ((fromInteger q)*) $ fmap snd $ findItem items s

findItem :: ItemList -> Code -> Maybe (Label,Price)
findItem (It (code,item) items) s | code == s = Just item
                                  | otherwise = findItem items s 
findItem Nil _ = Nothing

    
itemMap :: (Item -> Item) -> ItemList -> ItemList
itemMap f (It item items) = It (f item) (itemMap f items)
itemMap _ Nil = Nil

