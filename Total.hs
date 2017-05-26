module Total
where

type Price = Double
type Quantity = Integer
type Label = String
type Code  = String
type Item = (Code,(Label,Price))

total :: [Item] -> Quantity -> Code -> Maybe Price
total items q s = fmap ((fromInteger q)*) $ fmap snd $ lookup s items

readQuantity :: String -> Maybe Quantity
readQuantity s = case reads s of
    [(q,_)] -> Just q
    []      -> Nothing
