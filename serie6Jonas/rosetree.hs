data Rose a = Rose a [Rose a]
    deriving Show

instance Eq a => Eq (Rose a) where
    Rose x xs == Rose y ys = x == y && xs == ys

instance Ord a => Ord (Rose a) where
  compare (Rose x ts) (Rose y us) = case compare x y of
    EQ -> compare ts us
    cp -> cp


class Pretty a where
    pretty :: a -> String

instance Show a => Pretty (Rose a) where
    pretty rose = addelem rose 0
        where build []       _     string = string
              build (a : as) depth string = build as depth (string ++ concat (replicate depth "|   ") ++ "+-- " ++ addelem a (depth + 1))
              addelem (Rose a as) depth   = build as depth (show a ++ "\n")