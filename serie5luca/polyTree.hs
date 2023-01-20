data Tree a = Leaf a | Node (Tree a) (Tree a)

test = Node (Leaf (Leaf 1)) (Leaf (Leaf 1))


flatTree :: Tree (Tree a) -> Tree a
flatTree (Leaf x)                  = x
flatTree (Node (Leaf x) (Leaf y))  = Node x y
flatTree (Node (Leaf x) y)         = Node x (flatTree y)
flatTree (Node x (Leaf y))         = Node (flatTree x) y
flatTree (Node x y)                = Node (flatTree x) (flatTree y)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree func (Leaf l)   = Leaf (func l)
mapTree func (Node x y) = Node (mapTree func x) (mapTree func y)

foldTree   :: (a -> b) -> (b -> b -> b) -> Tree a -> b -- why f2?
foldTree f1 f2 (Node tree1 tree2) = f2 (foldTree f1 f2 tree1) (foldTree f1 f2 tree2)
foldTree f1 _ (Leaf a)            = f1 a

extendTree :: (a -> Tree b) -> Tree a -> Tree b
extendTree f (Node t1 t2) = Node (extendTree f t1) (extendTree f t2)
extendTree f (Leaf l)     = f l