{-
1. Identitätsfunktion auf Listen
2. Produkt aller elemente
3. subtraktion linksgeklammert
4. subtrakion mit rechts rechtsparität 
-}

-- wieso kann man hier li rauslassen?
myMap :: (a -> b) -> [a] -> [b]
myMap f li = foldr ((:) . f) [] li

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []