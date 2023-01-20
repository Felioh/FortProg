data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
    fmap f (Leaf a)   = Leaf (f a)
    fmap f (Node a b) = Node (fmap f a) (fmap f b)

instance Applicative Tree where
    pure = Leaf
    (<*>) (Leaf a) x   =  fmap a x
    (<*>) (Node a b) x = Node (a <*> x) (b <*> x)

instance Monad Tree where
    return = pure
    (>>=) (Leaf a) f   = f a
    (>>=) (Node a b) f = Node (a >>= f) (b >>= f)


{-

-- Functor laws
fmap id      x = id                x -- Identity
Für x = (Leaf a):
  fmap id (Leaf a)
= Leaf (id a)
= Leaf a
= id (Leaf a)

Für x = (Node treeA treeB):
  fmap id (Node treeA treeB)
= Node (fmap id treeA) (fmap id treeB)
= Node treeA treeB
= id (Node treeA treeB)

fmap (f . g) x = (fmap f . fmap g) x -- Composition
Für x = (Leaf a):
  fmap (f . g) (Leaf a)
= Leaf ((f . g) a))
= Leaf (f (g a))
= fmap f (Leaf (g a))
= fmap f (fmap g (Leaf a))
= (fmap f . fmap g) (Leaf a)



-- Applicative functor laws
pure id <*> v      = v          -- Identity
  (Leaf id) <*> v
= fmap id v
= id v
= v


pure f <*> pure x = pure (f x) -- Homomorphism

  pure f <*> pure x
= Leaf f <*> pure x
= fmap f (pure x)
= fmap f (Leaf x)
= Leaf (f x)
= pure (f x)

-}

