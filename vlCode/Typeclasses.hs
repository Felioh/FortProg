import Prelude hiding (elem)

-- Checks whether an element is contained in a list:
--elem :: Int -> [Int] -> Bool
--elem :: Char -> [Char] -> Bool
--elem :: Bool -> [Bool] -> Bool
elem :: Eq a => a -> [a] -> Bool
elem x []     = False
elem x (y:ys) = x == y || elem x ys

-- Eq is a type class: a collection of functions (here: (==), (/=))
{-
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  -- default definitions:
  x /= y = not (x == y)
  x == y = not (x /= y)

-}

-- A simple type:
data BW = Black | White
 deriving Show

instance Eq BW where
  Black == Black = True
  White == White = True
  Black == White = False
  White == Black = False
  
  x /= y = not (x == y)

instance Ord BW where
  compare Black Black = EQ
  compare Black White = LT
  compare White Black = GT
  compare White White = EQ

data Tree a = Empty | Node (Tree a) a (Tree a)
 deriving (Ord, Show, Read)

aTree :: Tree Int
aTree = Node Empty 42 Empty

instance Eq a => Eq (Tree a) where
  Empty         == Empty         = True
  Node l1 e1 r1 == Node l2 e2 r2 = e1 == e2 && l1 == l2 && r1 == r2
  _             == _             = False

  --x /= y = not (x == y)

testElemTree = elem Empty [aTree]


-- Data to express ordering relations:
--data Ordering = LT | EQ | GT
{-
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (>), (<=), (>=) :: a -> a -> Bool
  min, max :: a -> a -> a
  -- various default definitions
  -- minimal instance definition contains at least `compare` or (<=)
-}

{-
class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]  -- predefined

reads :: Read a => ReadS a
reads = readsPrec 0

read :: Read a => String -> a
read str = case reads str of
             [(x,"")] -> x
             _        -> error "no parse"
-}

f x = 42

loop = loop
