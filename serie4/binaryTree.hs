import SimplePrelude

data Tree a = End | Node a (Tree a) (Tree a)
    deriving Show

testBaum = Node 2 (Node 3 End End) (Node 5 End End)

sumTree1 :: Tree Int -> Int
sumTree1 End = 0
sumTree1 (Node num l_tree r_tree) = num + sumTree1 l_tree + sumTree1 r_tree

sumTree2 :: Tree Int -> Int
sumTree2 tree = add tree 0
    where add End n = n
          add (Node num l_tree r_tree) n = add l_tree (n+num) + add r_tree 0
          
          


mirrorTree :: Tree a -> Tree a
mirrorTree End = End
mirrorTree (Node a l_tree r_tree) = Node a (mirrorTree r_tree) (mirrorTree l_tree)



toList :: Tree a -> [a] 
toList (Node value l_tree r_tree) = append [] (Node value l_tree r_tree)
    where append ls End = ls
          append ls (Node value l_tree r_tree) = append (value : ls) l_tree ++ append [] r_tree

