import Data.Ratio

fibonacci :: [Integer]
fibonacci = fib 0 1
            where fib fibo fibo1 = fibo : fib fibo1 (fibo1 + fibo)

goldenRatio :: [Rational]
goldenRatio = calc 0
              where calc n = 1 + (fibonacci !! n) % (fibonacci !! (n+1)) : calc (n+1)


approx :: Rational -> [Rational] -> Rational
approx eps xs = calc eps xs
                where calc eps (y:x:xs) = if abs (y - x) <= eps then x
                                                                else calc eps (x:xs)



