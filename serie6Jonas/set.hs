type IntSet = Int -> Bool

lostSet :: IntSet
lostSet x = elem x [4, 8, 15, 16, 23, 42]

set2 :: IntSet
set2 x = elem x [4, 8, 15, 16, 23, 42,13]

empty :: IntSet
empty = \x -> False 

insert :: Int -> IntSet -> IntSet
insert x set = \y -> (y == x) || (set y)

remove :: Int -> IntSet -> IntSet
remove x set = \y -> not (y==x) && set y
        

isElem :: Int -> IntSet -> Bool
isElem x set = set x

union :: IntSet -> IntSet -> IntSet
union set1 set2 = \y -> set1 y || set2 y

intersection :: IntSet -> IntSet -> IntSet
intersection set1 set2 = \y -> set1 y && set2 y

difference :: IntSet -> IntSet -> IntSet
difference set1 set2 = \y -> set1 y && not (set2 y)

complement :: IntSet -> IntSet
complement set = \y -> not (set y)

listToSet :: [Int] -> IntSet
listToSet list = \y -> elem y list

