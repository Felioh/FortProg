data Child = Naughty | Nice
    deriving Show

data Wish a = Wish a
    deriving Show 

data Present a = Box a | WrappingPaper (Present a)
    deriving Show 

instance Eq Child where
  Naughty == Naughty = True 
  Nice == Nice       = True 
  _ == _             = False

wrap :: Int -> a -> Present a
wrap 0 x = Box x
wrap n x = WrappingPaper (wrap(n - 1) x)

santa :: [(Child, Wish a)] -> [(Child, Maybe (Present a))]
santa xs = snd (foldr santasLittleHelper (0, []) xs)
  where santasLittleHelper = \(c, Wish x) (n,cs) ->
          let c' = if c == Naughty then (c, Nothing) else (c, Just (wrap n x))
          in (n + 1, c':cs)