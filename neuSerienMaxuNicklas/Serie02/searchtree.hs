
data SearchTree  = Empty | Node Int  SearchTree  SearchTree
    deriving (Show)



testNode :: SearchTree
testNode = Node 69 (Node 42 Empty Empty) (Node 188 (Node 88 Empty Empty) (Node 1337 Empty Empty))

insert :: SearchTree -> Int -> SearchTree
insert  Empty newInt = Node newInt Empty Empty
insert (Node nodeInt leftTree rightTree ) newInt | newInt < nodeInt  = Node nodeInt (insert leftTree newInt) rightTree
                                                 | newInt >= nodeInt = Node nodeInt leftTree (insert rightTree newInt)
                                                 | otherwise         = Node 42 leftTree rightTree



isElem :: SearchTree -> Int -> Bool
isElem Empty searchInt = False
isElem (Node nodeInt leftTree rightTree) searchInt | nodeInt == searchInt = True
                                                   | searchInt < nodeInt  = isElem leftTree searchInt
                                                   | searchInt > nodeInt  = isElem rightTree searchInt
                                                   | otherwise  = False


delete :: SearchTree -> Int -> SearchTree
delete Empty deleteInt = Empty
delete (Node nodeInt leftTree rightTree ) deleteInt | deleteInt == nodeInt = Empty
                                                    | deleteInt < nodeInt  = Node nodeInt (delete leftTree deleteInt) rightTree
                                                    | deleteInt > nodeInt   = Node nodeInt leftTree (delete rightTree deleteInt)
                                                    | otherwise = Node 1337 Empty Empty