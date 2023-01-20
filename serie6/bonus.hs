-- bonus aufgabe 5

-- für IDE damit sie nicht meckert
f :: (a -> b) -> (a -> b)
f a = a

g :: (a -> b) -> (a -> b)
g a = a

h :: (a -> b) -> (a -> b)
h x = x

-- 1.

-- \x y -> f (g x) y

func1 :: (a -> b) -> a -> b
func1 x y = (f . g) x y

-- \x y -> f (g (h x y))
func2 :: (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> b
func2 x y = f . g . h x y

-- \f g x -> g (f x)
func3 :: (a -> b) -> (b -> c) -> a -> c
func3 f g x = (g . f) x


--2.

-- flip id

flipID :: b -> (b -> c) -> c
flipID = \x f -> f x

-- (.).(.) 

eule :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
eule = \f1 f2 a1 a2 -> f1 (f2 a1 a2)
