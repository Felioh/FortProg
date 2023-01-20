import Test.QuickCheck

data Set a = Set [a]

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
--------------------------------------------------------------------
--Aufgabe
--delete
prop_delete1:: Eq a => Set a -> a -> Bool 
prop_delete1 set x = not (member x (delete x set)) 

prop_delete2:: Eq a => a -> Bool
prop_delete2 x = isEmpty (delete x (Set [x]))

--intersect
prop_intersect1:: Ord a => Set a -> Bool
prop_intersect1 set = not (isEmpty (intersect set set)) 

prop_intersect2:: Ord a => Set a -> Bool
prop_intersect2 set = isEmpty (intersect set empty)

--size
prop_size1:: Bool
prop_size1 = size (empty) == 0

prop_size2:: Eq a => Set a -> a -> Bool
prop_size2 set x | (member x set) = (size set)  == size (insert x set)
                 | otherwise      = (size set) + 1 == size (insert x set) 


