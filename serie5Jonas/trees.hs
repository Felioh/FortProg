data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

flatTree   :: Tree (Tree a) -> Tree a
flatTree (Node (Leaf treel) (Leaf treer))       = Node treel            treer
flatTree (Node treel (Leaf treer))              = Node (flatTree treel) treer
flatTree (Node (Leaf treel) treer)              = Node treel            (flatTree treer)
flatTree (Node treel treer)                     = Node (flatTree treel) (flatTree treer)
flatTree (Leaf tree)                            = tree

testtree = Node (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)) (Node (Leaf 1) (Leaf 4))


mapTree    :: (a -> b) -> Tree a -> Tree b
mapTree f (Node l r)  = Node (mapTree f l) (mapTree f r)
mapTree f (Leaf x)    = Leaf (f x) 


foldTree   :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree f1 f2 (Leaf x)         = f1 x 
foldTree f1 f2 (Node l r)       = f2 (foldTree f1 f2 l) (foldTree f1 f2 r)

extendTree :: (a -> Tree b) -> Tree a -> Tree b
extendTree f (Leaf x)   = f x
extendTree f (Node l r) = Node (extendTree f l) (extendTree f r)