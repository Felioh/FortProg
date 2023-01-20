import SimplePrelude

-- Wiederholung:

-- Knotenbeschrifteter Baum:
--data SearchTree a = Empty | Branch (SearchTree a) a (SearchTree a)

-- Blattbeschrifteter Baum:
--data Tree a = Leaf a | Node (Tree a) (Tree a)

inits :: [a] -> [[a]]
inits xs = map reverse (reverse (tails (reverse xs)))

inits' :: [a] -> [[a]]
inits' xs = help (reverse (tails (reverse xs)))
  where help []     = []
        help (l:ls) = reverse l : help ls

-- inits [1,2,3] -> [[], [1], [1,2], [1,2,3]]
-- inits [3,2,1] -> [[], [3], [3,2], [3,2,1]]
--                  [[3,2,1], [3,2], [3], []]
-- tails [1,2,3] -> [[1,2,3], [2,3], [3], []]

tails :: [a] -> [[a]]
tails []     = [[]]
tails (x:xs) = (x:xs) : tails xs

{-map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs-}

{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f e []     = e
foldr f e (x:xs) = f x (foldr f e xs)
  
  foldr f e (x1 : x2 : ... : [])
= foldr f e ((:) x1 ((:) x2 ((:) ... [])))
=           (f   x1 (f   x2 (f   ... e)))

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f e []     = e
foldl f e (x:xs) = foldl f (f e x) xs

  foldl f e (x1 : x2 : ... : [])
= foldl f e ((:) x1 ((:) x2 ((:) ... [])))
= (f (f (f e x1) x2) ...)
-}

idList :: [a] -> [a]
--idList xs = foldr (:) [] xs
idList xs = foldr (\x xs -> x : xs) [] xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x xs -> f x : xs) [] xs

sum' []     = 0
sum' (x:xs) = x + sum' xs

sum :: [Int] -> Int
sum xs = foldr (+) 0 xs

maximum' :: [Int] -> Int
maximum' [x]    = x
maximum' (x:xs) = let max = maximum' xs
                  in if x > max then x else max

maximum'' :: [Int] -> Int
maximum'' (x:xs) = maximumAcc xs x
  where maximumAcc []     acc = acc
        maximumAcc (x:xs) acc = if x > acc then maximumAcc xs x
                                           else maximumAcc xs acc

{-
  foldr f e (x1 : x2 : ... : [])
= foldr f e ((:) x1 ((:) x2 ((:) ... [])))
=           (max x1 (max x2 (max ... 0)))
-}
maximum :: [Int] -> Int
maximum (x:xs) = foldr max x xs
  where max :: Int -> Int -> Int
        max x y = if x > y then x else y

{-
data [a] = [] | (:) a [a]

foldList :: b -> (a -> b -> b) -> [a] -> b
foldList nil cons []     = nil
foldList nil cons (x:xs) = cons x (foldList nil cons xs)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f e []     = e
foldr f e (x:xs) = f x (foldr f e xs)

foldr :: (a -> b -> b) -> b -> [a] -> b

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b
-}

foldMaybe :: b -> (a -> b) -> Maybe a -> b
foldMaybe nothing just Nothing  = nothing
foldMaybe nothing just (Just x) = just x

{-
foldMaybe nothing just maybe = case maybe of
                                 Nothing -> nothing
                                 Just x  -> just x
-}


foldEither :: (a -> c) -> (b -> c) -> Either a b -> c
foldEither left right (Left x)  = left x
foldEither left right (Right y) = right y

data Tree a b = Empty
              | Leaf a
              | Node b [Tree a b]
  deriving Show

foldTree :: c -> (a -> c) -> (b -> [c] -> c) -> Tree a b -> c
foldTree empty leaf node tree = case tree of
  Empty        -> empty
  Leaf x       -> leaf x
  Node y trees -> node y (map (\tree -> foldTree empty leaf node tree) trees)
