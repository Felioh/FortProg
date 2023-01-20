data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Functor Tree where
    fmap = treemap

treemap:: (a -> b) -> Tree a -> Tree b
treemap f (Leaf a)   = Leaf (f a)
treemap f (Node l r) = Node (treemap f l) (treemap f r)

instance Applicative Tree where 
    pure x = Leaf x

    (Leaf x)   <*> t   = fmap x t
    (Node l r) <*> t   = Node (l <*> t) (r <*> t)


--return x ist gleich pure
-- 1. mx auswerten 2. auf das Ergebnis von mx wird f angewendet, also f (mx) -> daraus resultieren die beiden Fälle.
instance Monad Tree where 
    return x = Leaf x
    --return x = pure x

    mx  >>= f = case mx of (Leaf a)   -> f a
                           (Node l r) -> Node (l >>= f) (r >>= f)

