data Tree a =  Leaf a | Node (Tree a) (Tree a)
    deriving Show

sumTree1:: Tree Integer -> Integer
sumTree1 (Leaf x)   = x
sumTree1 (Node l r) = sumTree1 l + sumTree1 r

sumTree2:: Tree Integer -> Integer
sumTree2 tree = calc tree 0 
        where calc (Node l r) sum = (calc l  0) + (calc r 0) + sum
              calc (Leaf x)   sum =  x + sum


mirrorTree:: Tree a -> Tree a
mirrorTree (Leaf a )    = Leaf a
mirrorTree (Node l r)   = (Node (mirrorTree r) (mirrorTree l)) 


toList:: Tree a -> [a]
toList (Leaf a )    = [a]
toList (Node l r)   = toList l ++ toList r

baumo = Node (Node (Leaf 2) (Node (Node (Leaf 11) (Leaf 6)) (Leaf 4))) (Leaf 10)