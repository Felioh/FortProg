pure :: Monad m => a -> m a
pure = return

(<*>) :: Monad m => m (a -> b) -> m a ->  m b
ff <*> fx = ff >>= \f -> fx >>= \x -> return (f x)


{- -- dürfen verwendet werden:
fmap id              = id                      --Identity
fmap (f . g)         = fmap f . fmap g         --Composition 
return x  >>= f      = f x                     -- Left Identity
m         >>= return = m                       -- Right Identity
(m >>= f) >>= g      = m >>= (\x -> f x >>= g) -- Associativity
-}

  pure id <*> v
= return id >>= \f -> v >>= \x -> return (f x) -- Einsetzen der Implementierung
= (\f -> v >>= \x -> return (f x)) id          -- Anwendung Left Identity
= v >>= \x -> return (id x)                    -- Anwendung des äußeren Lambdas
= v >>= \x -> return x                         -- Anwendung id
= v >>= return                                 -- Eta-Reduktion
= v                                            -- Anwendung Right Identity


  pure g  <*> pure v                                 -- Einsetzen der Implementierung
= return g >>= \f -> return v >>= \x -> return (f x) -- Anwendung Left Identity
= (\f -> return v >>= \x -> return (f x)) g          -- Anwendung des äußeren Lambdas
= return v >>= \x -> return (g x)                    -- Anwendung Left Identity
= (\x -> return (g x)) v                             -- Anwendung des Lambdas
= return (g v)                                       -- Einsetzen der Implementierung (umgekehrt)
= pure (g v)
