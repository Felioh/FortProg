data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show


--FUNCTOR
instance Functor Tree where 
    fmap = treeMap

treeMap:: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x)   = Leaf (f x)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

--APPLICATIVE
instance Applicative Tree where
    pure a = Leaf a

    (Leaf a)   <*> t = fmap a t 
    (Node l r) <*> t = Node (l <*> t) (r <*> t)  

-- MONAD
instance Monad Tree where                       --VERSTEHEN!!!!!
    return x = Leaf x

    mx >>= f = case mx of (Leaf x) -> f x
                          (Node l r) -> Node (l >>= f) (r >>= f)

--Aufgabenteil b

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure = Just

  (Just f) <*> (Just x) = Just (f x)
  _        <*> _        = Nothing

-- Aufgabe
fmap id           = id                   --Identity
x = Nothing
fmap id x
= fmap id Nothing
= Nothing 
= id Nothing 
= id x

x = Just v
fmap id x
= fmap id (Just v)
= Just id v
= Just v
= id (Just v)
= id x


fmap (f . g)      = fmap f . fmap g      --Composition
x = Nothing
fmap (f . g) Nothing 
= Nothing
=


pure id <*> v     = v                    --Identity
pure f <*> pure x = pure (f x)           --Homomorphismus

