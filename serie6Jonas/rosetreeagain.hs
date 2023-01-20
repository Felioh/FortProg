data Rose a = Rose a [Rose a]


instance Eq a => Eq (Rose a) where
    Rose x xs == Rose y ys = x == y && xs == ys

instance Ord a => Ord (Rose a) where
    Rose x xs < Rose y ys = x < y || (x == y && xs < ys )
    Rose x xs > Rose y ys = x > y || (x == y && xs > ys )



class Pretty a where
  pretty :: a -> String

instance Pretty a => Pretty (Rose a) where
  pretty t = prettyTree 0 t
    where
    prettyTree n (Rose x ts) = intercalate "\n"
                             $ prettyIndented n x : map (prettyTree (n + 1)) ts
    prettyIndented 0 x = pretty x
    prettyIndented n x = concat (replicate (n - 1) "|   ") ++ "+-- " ++ pretty x

intercalate :: [a] -> [[a]] -> [a]
intercalate _   []     = []
intercalate _   [x]    = x
intercalate sep (x:xs) = concat [x, sep, intercalate sep xs]

instance Pretty Int where
  pretty n = show n

instance Pretty Bool where
  pretty b = show b