-- Serie 3, Binomialkoeffizient und Pascalsches Dreieck

import SimplePrelude

-- Computes the factorial of a number.
fac :: Int -> Int
fac n = if n == 0 then 1 else n * fac (n - 1)

-- Computes binomial coefficient
binom :: Int -> Int -> Int
binom n k = div (fac n) ((fac k) * fac(n-k))

