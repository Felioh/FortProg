
-- rose tree
data Rose a = Rose a [Rose a]
  deriving Show
-- test aus der aufgabe
test = putStrLn (pretty (Rose 4 [Rose 5 [Rose 1 [], Rose 2 [Rose 7 [], Rose 8 []], Rose 3 []], Rose 6 []]))

instance Ord a => Eq (Rose a) where
   --(==) (Rose a ls1) (Rose b ls2) = a == b && ls1 == ls2
   (==) rose1 rose2 = case compare rose1 rose2 of -- <- das ist für die lappen ;)
                        EQ -> True
                        _ -> False

instance Ord a => Ord (Rose a) where
    compare (Rose a ls1) (Rose b ls2) = case compare a b of
                                        LT -> LT    -- less than - definiert in compare
                                        GT -> GT    -- greater than -- definiert in compare
                                        EQ -> compare ls1 ls2

class Pretty a where
    pretty :: a -> String

instance Show a => Pretty (Rose a) where
    pretty rose = addelem rose 0
        where build []       _     string = string
              build (a : as) depth string = build as depth (string ++ concat (replicate depth "|   ") ++ "+-- " ++ addelem a (depth + 1))
              addelem (Rose a as) depth = build as depth (show a ++ "\n")
      
      