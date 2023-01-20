type IntSet = Int -> Bool
    

lostSet :: IntSet
lostSet = \x -> elem x [4, 8, 15, 16, 23, 42]


empty :: IntSet
empty = \x -> False

{- -- Empty number set
empty :: IntSet
empty = \_ -> False -}

insert :: Int -> (Int -> Bool) -> (Int -> Bool)
--insert x set = \x -> elem x [x, 4, 8, 15, 16, 23, 42]
--insert x set = \x -> (elem x set) = True 
insert x set = \y -> y == x || set y


remove :: Int -> IntSet -> IntSet
remove x set = \y -> x /= y && set y

isElem :: Int -> IntSet -> Bool
isElem x set = set x

union :: IntSet -> IntSet -> IntSet
union set set1 = \y -> set y || set1 y

intersection :: IntSet -> IntSet -> IntSet
intersection set set1 = \y -> set y && set1 y

difference :: IntSet -> IntSet -> IntSet
difference set set1 = \y -> set y && not (set1 y) 

complement :: IntSet -> IntSet
complement set = \y -> not (set y)

listToSet :: [Int] -> (Int -> Bool)
listToSet list = \x -> elem x list 
