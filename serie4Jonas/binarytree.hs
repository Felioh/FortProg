data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show 

sumTree1 :: Tree Int -> Int
sumTree1 (Leaf x)   = x
sumTree1 (Node l r) = sumTree1 l + sumTree1 r

sumTree2 :: Tree Int -> Int
sumTree2 (Leaf x) = x
sumTree2 baum     = calc 0 baum
            where calc sum (Leaf x)   = sum + x
                  calc sum (Node l r) = calc sum l + calc 0 r

baumo :: Tree Int
baumo = Node (Node (Leaf 2) (Leaf 5)) (Node (Node (Leaf 7) (Leaf 8)) (Leaf 12))


mirrorTree:: Tree a -> Tree a
mirrorTree (Leaf x)   = Leaf x
mirrorTree (Node l r) = Node (mirrorTree r) (mirrorTree l)

toList:: Tree a -> [a]
toList (Leaf x)   = [x]
toList (Node l r) = toList l ++ toList 