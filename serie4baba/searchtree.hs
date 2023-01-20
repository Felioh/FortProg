data SearchTree = Empty | Node Int SearchTree SearchTree
    deriving Show
    
insert :: SearchTree -> Int -> SearchTree
insert Empty             inti = Node inti Empty Empty
insert (Node knoten l r) inti | inti <= knoten = Node knoten (insert l inti) r
                              | inti > knoten = Node knoten l (insert r inti) 
                              
isElem :: SearchTree -> Int -> Bool 
isElem Empty inti             = False
isElem (Node knoten l r) inti | inti < knoten = isElem l inti
                              | inti > knoten  = isElem r inti
                              | inti == knoten = True

delete :: SearchTree -> Int -> SearchTree
delete (Node knoten left (Node knotenR rl rr)) inti    | inti <  knoten = Node knoten (delete left inti) (Node knotenR rl rr)
                                                       | inti >  knoten = Node knoten left (delete (Node knotenR rl rr) inti) 
                                                       | inti == knoten = Node knotenR (inserti rl left)  rr
                                                                        where  inserti Empty                left = left
                                                                               inserti (Node inty Empty lr) left = Node inty left lr
                                                                               inserti (Node inty ll lr)    left = Node inty (inserti ll left) lr
                                                                                                                    
                                                                             


baumo =  8 (Node 5 Empty Empty) (Node 10 Empty Empty)


{-                        8
             5                      30 
         2       7            15          40 -}