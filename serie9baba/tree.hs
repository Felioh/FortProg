data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show



instance Functor Tree where
    fmap f (Leaf x)   = Leaf (f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)



instance Applicative Tree where
  pure   a         = Leaf a
  (Leaf   f) <*> t = fmap f t
  (Node l r) <*> t = Node (l <*> t) (r <*> t)   --Felix HILFE
  
--fmap :: (a -> b) -> f a -> f b 
--pure :: a -> f a  
-- (<*>) :: f (a -> b) -> f a -> f b


instance Monad Tree where
  -- return :: a -> Tree a
  return x = Leaf x
  -- (>>=) :: Tree a -> (a -> Tree b) -> Tree b
  (Leaf   x) >>= f = f x
  (Node l r) >>= f = Node (l >>= f) (r >>= f)