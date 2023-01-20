{- Zunächst geben wir die Programmsignatur an.

Σ=(S,F)
S={Int, IntSet, Bool}
F={
empty :: IntSet
isEmpty :: IntSet -> Bool
member :: Int -> IntSet -> Bool
insert :: Int -> IntSet -> IntSet
union :: IntSet -> IntSet -> IntSet
-----------------------
delete :: IntSet -> Int -> IntSet
intersect:: IntSet -> IntSet -> IntSet
size :: Intset -> Int
-----------------------
}

Dann definieren wir die Variablenmenge X.
X={s1 :: IntSet, s2 :: IntSet, x :: Int, y :: Int}

Und schließlich geben wir die Gleichungsmenge an, wobei wir etwaige Vorbedingungen explizit notieren.
E={
isEmpty empty = True
member x empty = False
size empty = 0
isEmpty (insert x s1) = False
member x (insert x s1) = True
member y (insert x s1) = member y s1, falls x /= y
size (insert x s1) = size s1 + (if member x s1 then 0 else 1)
isEmpty (delete x s1) = True, falls s1 = empty
member x (delete x s1) = False
member y (delete x s1) = member y s1, falls x /= y
size (delete x s1) = size s1 - (if member x s1 then 1 else 0)
isEmpty (union s1 s2) = isEmpty s1 && isEmpty s2
member x s1 || member x s2 = member x (union s1 s2)
size (union s1 s2) = size s1 + size s2 - size (intersect s1 s2)
isEmpty (intersect empty s2) = True
isEmpty (intersect s1 empty) = True
member x s1 && member x s2 = member x (intersect s1 s2)
size (intersect s1 s2) >= 0 && size (intersect s1 s2) <= size s1 && size (intersect s1 s2) <= size s2 = True
} -}
import Test.QuickCheck

data Set a = Set [a]
    deriving Show

--a muss dafür arbitrary sein
instance Arbitrary a => Arbitrary (Set a) where 
    arbitrary = do xs <- arbitrary 
                   return (Set xs)

empty :: Set a
empty = Set []

isEmpty :: Set a -> Bool
isEmpty (Set xs) = null xs

insert :: a -> Set a -> Set a
insert x (Set xs) = Set (x:xs)

member :: Eq a => a -> Set a -> Bool
member x (Set xs) = elem x xs

delete :: Eq a => a -> Set a -> Set a
delete x (Set xs) = Set (remove x xs)
  where
    remove _ []                 = []
    remove y (z:zs) | y == z    = zs
                    | otherwise = remove y zs

union :: Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (xs ++ ys)

intersect :: Ord a => Set a -> Set a -> Set a
intersect (Set s1) (Set s2) = Set (merge s1 s2)
  where
    merge []     ys     = ys
    merge xs     []     = xs
    merge (x:xs) (y:ys) = case compare x y of
      LT -> x : y : merge xs ys
      EQ -> y : merge xs ys
      GT -> y : x : merge xs ys

size :: Set a -> Int
size (Set xs) = length xs


main :: IO ()
main = do
  putStr "prop_isEmpty_empty    : "
  quickCheck prop_isEmpty_empty
  putStr "prop_member_empty     : "
  quickCheck prop_member_empty
  putStr "prop_size_empty       : "
  quickCheck prop_size_empty
  putStr "prop_isEmpty_insert   : "
  quickCheck prop_isEmpty_insert
  putStr "prop_size_union       : "
  quickCheck prop_size_union



prop_isEmpty_empty:: Bool
prop_isEmpty_empty = isEmpty empty

prop_member_empty:: Int -> Bool
prop_member_empty x = not (member x empty)

prop_size_empty:: Bool 
prop_size_empty = size empty == 0

prop_isEmpty_insert:: Int -> Set Int -> Bool 
prop_isEmpty_insert x s1 = not (isEmpty (insert x s1)) 

--size (union s1 s2) = size s1 + size s2 - size (intersect s1 s2)
prop_size_union:: Set Int -> Set Int -> Bool 
prop_size_union s1 s2 = size(union s1 s2) == size s1 + size s2 - size (intersect s1 s2)
--isEmpty (intersect empty s2) = True
--isEmpty (intersect s1 empty) = True
--member x s1 && member x s2 = member x (intersect s1 s2)
--size (intersect s1 s2) >= 0 && size (intersect s1 s2) <= size s1 && size (intersect s1 s2) <= size s2 = True



