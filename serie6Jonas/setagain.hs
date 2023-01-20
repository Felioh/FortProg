type IntSet = Int -> Bool


lostSet :: IntSet
lostSet = \x -> elem x [4, 8, 15, 16, 23, 42]


empty :: IntSet
empty = \x -> False


insert :: Int -> IntSet -> IntSet
insert x set = \y -> set y || x == y

remove :: Int -> IntSet -> IntSet
remove x set = \y -> if x == y then False else set y