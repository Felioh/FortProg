data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show 

{- data Tree (Tree a) = Leaf (Tree a) | Node (Tree (Tree a)) (Tree (Tree a))
    deriving Show  -}


flatTree   :: Tree (Tree a) -> Tree a
flatTree (Node (Leaf treeL)            (Leaf treeR))        = Node treeL                            treeR
flatTree (Node (Node treeLL treeLR)    (Leaf treeR))        = Node (flatTree (Node treeLL treeLR))  treeR
flatTree (Node (Leaf treeL)            (Node treeRL treeRR))= Node treeL                            (flatTree (Node treeRL treeRR))
flatTree (Node (Node treeLL treeLR)    (Node treeRL treeRR))= Node (flatTree (Node treeLL treeLR))  (flatTree (Node treeRL treeRR))

mapTree    :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a)    = Leaf (f a)
mapTree f (Node l r)  = Node (mapTree f l) (mapTree f r)

foldTree   :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree f1 f2 (Node l r) = f2 (foldTree f1 f2 l) (foldTree f1 f2 r)
foldTree f1 f2 (Leaf a)   = f1 a 

extendTree :: (a -> Tree b) -> Tree a -> Tree b
extendTree f (Node l r)   = Node (extendTree f l) (extendTree f r)
extendTree f (Leaf a )    = f a
    


testtree = Node (Node (Node (Leaf(Leaf 1)) (Leaf (Leaf 2))) (Leaf(Leaf 3))) (Node (Leaf (Leaf 1)) (Leaf(Leaf 4)))




{- Node (Leaf 1) (Leaf 2) (Leaf 3)

Node (Node ll lr) (Node rl rr)
                             x
            x                                x
    x                x              x               x
1       2       3       4       5       6        7       8 -}