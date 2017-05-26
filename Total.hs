module Total
where

type Price = Double
type Quantity = Integer
type Label = String
type Code  = String
type Item = (Code,(Label,Price))

total :: [Item] -> String -> Code -> Maybe Price
total items q s = case readQuantity q of
    Just r -> fmap ((fromInteger r)*) $ fmap snd $ lookup s items
    Nothing -> Nothing

readQuantity :: String -> Maybe Quantity
readQuantity s = case reads s of
    [(q,_)] -> Just q
    []      -> Nothing
