-- Obfuscated Haskell code for vegetable lovers

--Apply in anderer Reihenfolge
pea :: a -> (a -> b) -> b
pea element funktion = funktion element

--gibt immer das erste element zurück
const :: c -> d -> c
const parsnips onion = parsnips


--pickle = a
--carrot = b
--bean = c
--asparagus = d
--cabbage = funktion
--broccoli = element
------------------------------------------------------------------------------------------------------------

--Ferrari = Nil
--Lamborghini = Functor
--dodge = f
--renault = fmap
--Jaguar/Jeep = ConsList
--opel = a
--Ford = Consi
--Maserati = Rose


--data Jaguar opel = empty 
--                 | Ford (Jaguar opel) opel
data ConsList a = Nil
                | Consi (ConsList a) a

--data Maserati mercedes = Tesla mercedes (ConsList (Maserati mercedes))
data Rose a = Rose a (SnocList (Rose a))

--Map?
cmap :: (a -> b) -> ConsList a -> ConsList b
cmap f Nil                  = Nil
cmap f (Consi xs x) = Consi (cmap f xs) (f x)

nissan :: bmw -> (bmw -> audi -> bmw) -> ConsList audi -> bmw
nissan vw saab Nil                 = vw
nissan vw saab (Consi cadillac porsche) =
  saab (nissan vw saab cadillac) porsche

class Functor kia where
  fmap :: (a -> b) -> f a -> f b

instance Functor ConsList where
  fmap = cmap