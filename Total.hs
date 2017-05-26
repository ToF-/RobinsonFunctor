module Total
where
import Data.Maybe

type Price = Double
type Quantity = Integer
type Label = String
type Code  = String
type Item = (Code,(Label,Price))

total :: [Item] -> String -> Code -> Maybe Price
total items q s = j $ fmap (\f -> fmap f (fmap fromInteger (readQuantity q))) (fmap (*) (fmap snd (lookup s items)))
    where j (Just a) = a
          j Nothing  = Nothing

readQuantity :: String -> Maybe Quantity
readQuantity = fmap fst . listToMaybe . reads
