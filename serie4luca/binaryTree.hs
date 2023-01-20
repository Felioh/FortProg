data Tree a = Leaf a | Node a (Tree a) (Tree a)
    deriving Show

sumTree1 :: Tree Int -> Int
sumTree1 (Leaf v) = v
sumTree1 (Node a l r) = sumTree1 l + sumTree1 r

sumTree2 :: Tree Int -> Int
sumTree2 tree = sumT 0 tree
    where sumT c (Leaf v) = c+v
          sumT c (Node a l r) = sumT (sumT c l) r

-- alle l und r tauschen
mirrorTree :: Tree a -> Tree a
mirrorTree (Leaf v)   = Leaf v
mirrorTree (Node a l r) = Node a (mirrorTree r) (mirrorTree l)

toList :: Tree a -> [a]
toList (Leaf v)     = [v]
toList (Node a l r) = toList l ++ toList r  

