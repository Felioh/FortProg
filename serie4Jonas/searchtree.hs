data SearchTree = Empty | Node Int SearchTree SearchTree
    deriving Show

insert:: Int -> SearchTree -> SearchTree
insert x Empty                           = Node x Empty Empty
insert x (Node knoten l r) | x <= knoten = Node knoten (insert x l) r
                           | otherwise   = Node knoten l (insert x r)

isElem:: Int -> SearchTree -> Bool 
isElem x Empty                           = False 
isElem x (Node knoten l r) | x == knoten = True 
                           | x < knoten  = isElem x l
                           | otherwise   = isElem x r

delete:: Int -> SearchTree -> SearchTree
delete x  Empty                                                = Empty
delete x (Node knoten Empty r) | x == knoten = r
                               | x <  knoten = (Node knoten Empty r)
                               | x >  knoten = (Node knoten Empty (delete x r))
delete x (Node knoten (Node leftKnoten ll rr) r) | x == knoten = Node leftKnoten ll (inserti r rr)
                                                 | x <  knoten = Node knoten (delete x (Node leftKnoten ll rr)) r
                                                 | x >  knoten = Node knoten (Node leftKnoten ll rr) (delete x r)
                          where inserti Empty                 baum  = baum
                                inserti (Node knoten Empty r) baum  = Node knoten baum r
                                inserti (Node knoten l r)     baum  = Node knoten (inserti baum l) r
                                                                                                         




baumo =  Node 8 (Node 5 Empty Empty) (Node 10 Empty Empty)


