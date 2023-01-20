import SimplePrelude

-- Blattbeschrifteter Baum
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

testtree = Node (Node (Node (Leaf(Leaf 1)) (Leaf (Leaf 2))) (Leaf(Leaf 3))) (Node (Leaf (Leaf 1)) (Leaf(Leaf 4)))

flatTree   :: Tree (Tree a) -> Tree a
flatTree (Node (Leaf tree1) (Leaf tree2)) = Node tree1 tree2
flatTree (Node (Leaf tree1) tree2)        = Node tree1 (flatTree tree2)
flatTree (Node tree1 (Leaf tree2))        = Node (flatTree tree1) tree2
flatTree (Node tree1 tree2) = Node (flatTree tree1) (flatTree tree2)


mapTree    :: (a -> b) -> Tree a -> Tree b
mapTree f (Node tree1 tree2) = Node (mapTree f tree1) (mapTree f tree2)
mapTree f (Leaf a)           = Leaf (f a)


foldTree   :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree f1 f2 (Node tree1 tree2) = f2 (foldTree f1 f2 tree1) (foldTree f1 f2 tree2)
foldTree f1 _ (Leaf a)            = f1 a


extendTree :: (a -> Tree b) -> Tree a -> Tree b
extendTree f (Node tree1 tree2) = Node (extendTree f tree1) (extendTree f tree2)
extendTree f (Leaf a) = f a
