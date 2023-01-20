{-double x = x + x

double (2 + 2):

 double
   |
(2 + 2)

   +
  / \
  \ /
(2 + 2)

   +
  / \
  \ /
   4

(2 + 2) + (2 + 2)

     +
    / \
(2+2) (2+2)

     +
    / \
    4 (2+2)

let x = 2+2
in x+x

let x = f 5
in x * x-}

{-
reverse :: [a] -> [a]
reverse []     = []
reverse [x]    = [x] -- überflüssig
reverse (x:[]) = [x] -- überflüssig (und syntaktischer Zucker)
reverse (x:xs) = reverse xs ++ [x]
-}

data BTree a = Leaf a
             | Node (BTree a) a (BTree a)
  deriving Show

foldBTree :: (a -> b) -> (b -> a -> b -> b) -> BTree a -> b
foldBTree fleaf fnode (Leaf x)     = fleaf x
foldBTree fleaf fnode (Node l n r) =
  fnode (foldBTree fleaf fnode l) n (foldBTree fleaf fnode r)

maxBTreeWithoutFold :: BTree Int -> Int
maxBTreeWithoutFold (Leaf x)     = x
maxBTreeWithoutFold (Node l n r) = max (max (maxBTreeWithoutFold l) n) (maxBTreeWithoutFold r)

testBTree :: BTree Int
testBTree = Node (Node (Leaf 4) 1 (Leaf 7)) 3 (Node (Leaf 2) 6 (Leaf 9))

maxBTree :: Ord a => BTree a -> a
maxBTree bt = foldBTree (\x -> x) (\maxl x maxr -> max (max maxl x) maxr) bt

{-
idList xs = foldr (:) [] xs
-}

idBTree bt = foldBTree (\x -> Leaf x) (\l x r -> Node l x r) bt
-- idBTree bt = foldBTree Leaf Node bt

replaceWithoutFold :: BTree a -> b -> BTree b
replaceWithoutFold (Leaf _)     y = Leaf y
replaceWithoutFold (Node l _ r) y =
  Node (replaceWithoutFold l y) y (replaceWithoutFold r y)

replace :: BTree a -> b -> BTree b
replace bt y = foldBTree (\_ -> Leaf y) (\ly _ ry -> Node ly y ry) bt

replaceMaxRecNaive :: BTree Int -> a -> (BTree a, Int)
replaceMaxRecNaive bt y = (replace bt y, maxBTree bt)

replaceMaxRec :: BTree Int -> a -> (BTree a, Int)
replaceMaxRec (Leaf x)     y = (Leaf y, x)
replaceMaxRec (Node l x r) y =
  let (ly, maxl) = replaceMaxRec l y
      (ry, maxr) = replaceMaxRec r y
  in (Node ly y ry, max (max maxl x) maxr)
{-replaceMaxRec (Node l x r) y = (Node ly y ry, max (max maxl x) maxr)
  where
    (ly, maxl) = replaceMaxRec l y
    (ry, maxr) = replaceMaxRec r y-}
{- Aber Achtung:
replaceMaxRec (Node l x r) y =
  let ly = fst (replaceMaxRec l y)
      maxl = snd (replaceMaxRec l y)
      ry = fst (replaceMaxRec r y)
      maxr = snd (replaceMaxRec r y)
  in (Node ly y ry, max (max maxl x) maxr)-}
{- Besser, aber nicht schön:
replaceMaxRec (Node l x r) y =
  let recl = replaceMaxRec l y
      ly = fst recl
      maxl = snd recl
      recr = replaceMaxRec r y
      ry = fst recr
      maxr = snd recr
  in (Node ly y ry, max (max maxl x) maxr)-}

replaceMaxRecWithFold :: BTree Int -> a -> (BTree a, Int)
replaceMaxRecWithFold bt y =
  foldBTree (\x -> (Leaf y, x))
            (\(ly, maxl) x (ry, maxr) -> (Node ly y ry, max (max maxl x) maxr))
            bt

replaceMaxNaive :: BTree Int -> BTree Int
replaceMaxNaive bt = replace bt (maxBTree bt)

replaceMax :: BTree Int -> BTree Int
replaceMax bt = bt'
  where (bt', m) = replaceMaxRecWithFold bt m