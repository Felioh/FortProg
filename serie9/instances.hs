data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show


instance Functor Tree where -- std funktion im container
    fmap f (Leaf a)           = Leaf (f a)
    fmap f (Node treeA treeB) = Node (fmap f treeA) (fmap f treeB)

instance Applicative Tree where -- container funktion im container
    pure = Leaf

    -- (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    (<*>) (Leaf a)           x = fmap a x 
    (<*>) (Node treeA treeB) x = Node (treeA <*> x) (treeB <*> x)

instance Monad Tree where -- fmap aber verkettbar
    return = pure

    (>>=) (Leaf a)           f = f a
    (>>=) (Node treeA treeB) f = Node (treeA >>= f) (treeB >>= f)



{-

-- Functor laws
fmap id x = id x -- Identity
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
