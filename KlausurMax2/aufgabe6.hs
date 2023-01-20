import Test.QuickCheck

data Number = Num [()]
    deriving (Show, Eq)

add :: Number -> Number -> Number
add (Num xs) (Num ys) = (Num (xs ++ ys))    


mult :: Number -> Number -> Number      
mult (Num xs) (Num (y:ys)) = add (Num xs) (mult (Num xs) (Num ys))



prop_add:: Number -> Number -> Bool
prop_add:: x y = add (Num xs) (Num ys) == add (Num ys) (Num xs)

