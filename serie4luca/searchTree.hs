data SearchTree = None | Node Int SearchTree SearchTree -- (value, left, right)
    deriving Show

insert :: SearchTree -> Int -> SearchTree
insert None n         = Node n None None
insert (Node v l r) n | n == v = Node v l r            -- case gleich
                      | n <  v = Node v (insert l n) r -- case links
                      | n >  v = Node v (insert r n) l -- case rechts 

isElem :: SearchTree -> Int -> Bool
isElem None n         = False
isElem (Node v l r) n = if v == n then 
                            True 
                        else if v < n then 
                            isElem l n 
                        else
                            isElem r n 
    
                                
delete :: SearchTree -> Int -> SearchTree
delete None n         = None
delete (Node v l r) n = if v == n then 
                            fixTree l r
                        else if v < n then 
                            delete l n 
                        else
                            delete r n
    where fixTree l None = l
          fixTree l (Node rv rl rr) = Node rv (fixTree l rl) rr -- alle "nach oben ziehen"

