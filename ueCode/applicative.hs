ata Error a = Failure String | Value a
  deriving Show

{-data Either a b = Left a | Right b

instance Functor (Either c) where
  -- fmap :: (a -> b) -> (Either c) a -> (Either c) b-}

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b
-}

{-
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)
-}

instance Functor Error where
  -- fmap :: (a -> b) -> Error a -> Error b
  fmap func (Failure s) = Failure s
  fmap func (Value x) = Value (func x)

{-
Identitätsgesetz: fmap id x = id x     für alle möglichen Werte vom Typ Error a

1. Fall: x = Failure str
fmap id x
= fmap id (Failure str)
= Failure str
= id (Failure str)
id x

2. Fall: x = Value y
fmap id x
= fmap id (Value y)
= Value (id y)
= Value y
= id (Value y)
id x

Kompositionsgesetz: fmap (f . g) x = (fmap f . fmap g) x      für alle möglichen Werte vom Typ Error a

1. Fall: x = Failure str
fmap (f . g) x
= fmap (f . g) (Failure str)
= Failure str
= fmap f (Failure str)
= fmap f (fmap g (Failure str))
= (.) (fmap f) (fmap g) (Failure str)
= (fmap f . fmap g) (Failure str)
(fmap f . fmap g) x

2. Fall: x = Value y
fmap (f . g) x
= fmap (f . g) (Value y)
= Value ((f . g) y)
= Value ((.) f g y)
= Value (f (g y))
= fmap f (Value (g y))
= fmap f (fmap g (Value y))
= (.) (fmap f) (fmap g) (Value y)
(fmap f . fmap g) (Value y)
-}

instance Applicative Error where
  -- pure :: a -> Error a
  pure = Value
  -- (<*>) :: Error (a -> b) -> Error a -> Error b
  (Failure str) <*> _             = Failure str
  (Value _)     <*> (Failure str) = Failure str
  (Value f)     <*> (Value x)     = Value (f x)

instance Monad Error where
  -- return :: a -> Error a
  return = Value
  -- (>>=) :: Error a -> (a -> Error b) -> Error b
  (Failure str) >>= _ = Failure str
  (Value x)     >>= f = f x

