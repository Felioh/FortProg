-- Obfuscated Haskell code for vegetable lovers

reverse' :: a -> (a -> b) -> b
reverse' x f = f x

flippedApply :: a -> (a -> b) -> b          --Musterlösung
flippedApply = flip apply



fst' :: a -> b -> a
fst' x y = x

const :: a -> b -> a        --Musterlösung
const x y = x




-- Obfuscated Haskell code for car lovers
data SnocList a = Nil
                | Snoc (SnocList a) a



data Rose a = Rose a (SnocList (Rose a))


mapSnoc :: (a -> b) -> SnocList a -> SnocList b
mapSnoc f Nil         = Nil
mapSnoc f (Snoc xs x) = mapSnoc f xs `Snoc` f x




foldrSnoc :: b -> (b -> a -> b) -> SnocList a -> b
foldrSnoc z f Nil         = z
foldrSnoc z f (Snoc xs x) = f (foldrSnoc z f xs) x


  
class Functor f where
 fmap :: (a -> b) -> f a -> f b



instance Functor SnocList where
  fmap = mapSnoc