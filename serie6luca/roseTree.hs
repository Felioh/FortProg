data Rose a = Rose a [Rose a]

instance Eq a => Eq (Rose a) where
    Rose x a == Rose y b = x == y && a == b

instance Ord a => Ord (Rose a) where
    compare (Rose a ls1) (Rose b ls2) = case compare a b of
                                        LT -> LT
                                        GT -> GT
                                        EQ -> compare ls1 ls2

tree = Rose 4 [Rose 5 [Rose 1 [], Rose 2 [Rose 7 [], Rose 8 []], Rose 3 []], Rose 6 []]

class Pretty a where
    pretty :: a -> String 

instance Show a => Pretty (Rose a) where
    pretty rose = addE rose 0
        where make []       _     s = s
              make (x : xs) d s = make xs d (s ++ concat (replicate d "|   ") ++ "+-- " ++ addE x (d + 1))
              addE (Rose x xs) d = make xs d (show x ++ "\n") 
