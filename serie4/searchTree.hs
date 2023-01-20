import SimplePrelude


data SearchTree = End | Node Int SearchTree SearchTree
  deriving Show

testBaum = Node 5 (Node 3 (Node 2 End End) End) End
testBaum2 = Node 5 End (Node 7 (Node 6 End End) (Node 8 End End))

insert :: Int -> SearchTree -> SearchTree
insert new End = Node new End End
insert new (Node wert left right) | new <= wert = Node wert (insert new left) right       -- go left
                                  | new >  wert = Node wert left (insert new right)       -- go right

-- Die Funktion isElem soll prüfen, ob eine gegebene Beschriftung in einem gegebenen Suchbaum enthalten ist.
isElem :: Int -> SearchTree -> Bool
isElem num End = False
isElem num (Node wert left right) | num == wert = True
                                  | num <  wert = isElem num left
                                  | num >  wert = isElem num right


-- Vergleich auf Typebende ist ein Problem
-- isElem :: Int -> SearchTree a -> Bool
-- isElem num (Node wert left right) | num == wert = True
--                                   | num <  wert && left   != End = isElem num left
--                                   | num >  wert && right  != End = isElem num right
--                                   | otherwise = False
-- isElem num (Node wert left End)

delete ::  Int -> SearchTree -> SearchTree
delete num End = End
delete num (Node wert left right) | num == wert = reStructure left right
                                  | num <  wert = Node wert (delete num left) right
                                  | num >  wert = Node wert left (delete num right)
  where reStructure l_tree End                          = l_tree
        reStructure l_tree (Node r_wert r_left r_right) = Node r_wert (reStructure l_tree r_left) r_right