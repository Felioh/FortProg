import Test.QuickCheck

data SearchTree = Empty | Branch SearchTree Int SearchTree
  deriving (Eq, Show)

insert :: Int -> SearchTree -> SearchTree
insert x Empty          = Branch Empty x Empty
insert x (Branch l n r) | x == n    = Branch l n r
                        | x <  n    = Branch (insert x l) n r
                        | otherwise = Branch l n (insert x r)

member :: Int -> SearchTree -> Bool 
member x Empty                   = False
member x (Branch l y r) | x == y = True
                        | x <  y = member x l
                        | x >  y =  member x r

klopfo:: SearchTree -> [Int]
klopfo Empty          = []
klopfo (Branch l x r) =  klopfo l ++ [x] ++ klopfo r

sort:: [Int] -> [Int]
sort []     = []
sort (x:xs) = (sort (filter (<=x) xs)) ++ [x] ++ (sort (filter (>x) xs))

isSorted:: [Int] -> Bool
isSorted (x:y:xs)  | x <= y    = isSorted  (y:xs)          
                   | otherwise = False
isSorted _                     = True

{- 
Wenn man ein Element in einen Suchbaum hinzufügt, so ist dieser danach nicht leer.
Wenn man ein Element in einen Suchbaum hinzufügt, dann ist dieses Element danach enthalten.
Wenn man einen Suchbaum flachklopft, dann ist die so entstandene Liste (aufsteigend) sortiert. -}
prop_insert1:: Int -> Bool
prop_insert1 x = insert x Empty /= Empty

prop_insert2::Int -> SearchTree -> Bool
prop_insert2 x tree = member x (insert x tree)

prop_klopfo:: SearchTree -> Bool 
prop_klopfo tree = isSorted(klopfo tree)



{- instance Arbitrary SearchTree where
    arbitrary =  do x <- arbitrary
                    l <- arbitrary
                    r <- arbitrary
                    oneof [return Empty, return (Branch l x r)] -}


instance Arbitrary SearchTree where
  arbitrary = do
    xs <- arbitrary
    return (foldr insert Empty (xs :: [Int]))


listo:: [Int]
listo =         do arbitrary
                   
                   
  

