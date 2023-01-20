{-#LANGUAGE TemplateHaskell#-}
import Test.QuickCheck

data Set a = Set [a]
    deriving Show
type IntSet = Set Int

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


prop_intersect :: IntSet -> IntSet -> Bool
prop_intersect a b = size (intersect a b) <= size a && size (intersect a b) <= size b

prop_union_size :: IntSet -> IntSet -> Bool
prop_union_size a b = size (union a b) >= size a && size (union a b) >= size b

prop_intersect2 :: IntSet -> IntSet -> Bool
prop_intersect2 a b = if isEmpty a || isEmpty b then isEmpty (intersect a b)
                                                else size (intersect a b) > 0

prop_size_delete:: Int -> IntSet -> Bool
prop_size_delete n a | member n a = size a - 1 == size (delete n (insert n a))
                     | otherwise  = size a     == size (delete n (insert n a))

prop_delete :: Int -> IntSet -> Bool
prop_delete a b = size (delete a (insert a b)) == size b

prop_delete2 :: Int -> IntSet -> Bool
prop_delete2 a xs = if member a xs then size (delete a xs) == (size xs) - 1
                                   else size (delete a xs) == size xs


instance Arbitrary a => Arbitrary (Set a) where
   -- arbitrary :: Gen (Set a)
   arbitrary = do
     xs <- arbitrary
     return (Set xs)

return []
testAll = $quickCheckAll