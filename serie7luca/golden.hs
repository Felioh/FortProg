import Data.Ratio ((%))

fib :: [Int]
fib = f 0 1
    where f a b = a : f b (a+b)

goldenRatio :: [Rational]
goldenRatio = f 0 1
    where f c n = (1+ (c % n)) : f n (c+n)

approx :: Rational -> [Rational] -> Rational
approx eps (a:b:bs) | abs (a - b) <= eps = b
                    | otherwise          = approx eps (b:bs)

