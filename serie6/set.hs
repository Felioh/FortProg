type IntSet = Int -> Bool

lostSet :: IntSet
lostSet x = elem x [4, 8, 15, 16, 23, 42]

empty :: IntSet
empty _ = False

insert :: Int -> IntSet -> IntSet
insert x set = \y -> y == x || set y

remove :: Int -> IntSet -> IntSet
remove x set = \y -> y /= x && set y

isElem :: Int -> IntSet -> Bool
isElem x set = set x

union :: IntSet -> IntSet -> IntSet -- MENGE = A oder B
union set1 set2 = \x -> set1 x || set2 x

intersection :: IntSet -> IntSet -> IntSet -- MENGE = A und B
intersection set1 set2 = \x -> set1 x && set2 x

difference :: IntSet -> IntSet -> IntSet -- MENGE = A ohne B
difference set1 set2 = \x -> set1 x && not (set2 x)

complement :: IntSet -> IntSet -- MENGE = alles außer A
complement set1 = \x -> not (set1 x)

-- complement lostSet 12
listToSet :: [Int] -> IntSet --geht relativ einfach, da Listen im Gegensatz zu Mengen nur endlich groß sein können
listToSet ls= \x -> elem x ls

--setToList :: IntSet -> [Int] --geht wegen der unendlichkeit von Mengen nicht..
