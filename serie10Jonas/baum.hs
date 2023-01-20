import Test.QuickCheck

data SearchTree = Empty | Branch SearchTree Int SearchTree
  deriving (Eq, Show)


instance Arbitrary SearchTree where
    arbitrary = do xs <- arbitrary
                   return (foldr insert Empty (xs :: [Int]))
{-     arbitrary = do l <- arbitrary 
                   x <- arbitrary 
                   r <- arbitrary 
                   oneof [return (Branch l x r), return Empty] -}

insert :: Int -> SearchTree -> SearchTree
insert x Empty          = Branch Empty x Empty
insert x (Branch l n r)
  | x == n    = Branch l n r
  | x <  n    = Branch (insert x l) n r
  | otherwise = Branch l n (insert x r)


isEmpty:: SearchTree -> Bool 
isEmpty Empty = True 
isEmpty _     = False

isElem:: Int -> SearchTree -> Bool 
isElem x Empty          = False 
isElem x (Branch l n r) | x == n = True 
                        | otherwise = isElem x l || isElem x r


flatten:: SearchTree -> [Int]
flatten Empty        = []
flatten (Branch l x r) = flatten l ++ [x] ++ flatten r


isSorted:: [Int] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)



main :: IO ()
main = do putStrLn "prop_isEmpty_insert:  "
          quickCheck prop_isEmpty_insert
          putStrLn "prop_insert_isElem:  "
          quickCheck prop_insert_isElem
          putStrLn "prop_flat_isSorted:  "
          quickCheck prop_flat_isSorted


prop_isEmpty_insert:: Int -> SearchTree -> Bool 
prop_isEmpty_insert x s1 = not (isEmpty (insert x s1))

prop_insert_isElem:: Int -> SearchTree -> Bool 
prop_insert_isElem x tree = isElem x (insert x tree)

prop_flat_isSorted:: SearchTree -> Bool 
prop_flat_isSorted tree = isSorted (flatten tree )

--Wenn man einen Suchbaum flachklopft, dann ist die so entstandene Liste (aufsteigend) sortiert.