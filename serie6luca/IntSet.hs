type IntSet = Int -> Bool

lostSet :: IntSet
lostSet = \x -> elem x [4, 8, 15, 16, 23, 42]

empty :: IntSet
empty _ = False

insert :: Int -> IntSet -> IntSet
insert x set = \y -> y == x || set y

remove :: Int -> IntSet -> IntSet
remove x set = \y -> y /= x && set y

isElem :: Int -> IntSet -> Bool
isElem x set = set x 

union :: IntSet -> IntSet -> IntSet
union a b =  \y -> a y || b y 

intersection :: IntSet -> IntSet -> IntSet
intersection a b = \y -> a y && b y

difference :: IntSet -> IntSet -> IntSet
difference a b = \y -> a y /= b y

complement :: IntSet -> IntSet
complement set = \y -> not(set y) 




listToSet :: [Int] -> IntSet
listToSet xs = \y -> elem y xs

-- Andersrum geht das nicht, weil Sets unendlich sein können und keine Reihenfolge haben.