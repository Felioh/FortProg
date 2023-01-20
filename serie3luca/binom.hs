-- factorial from factorial.hs
fac :: Integer -> Integer
fac n = if n == 0 then 1 else n * fac (n - 1)

-- binomialcoefficient (n over k)
binom :: Integer -> Integer -> Integer
binom n k = div (fac n)  ((fac k) * fac (n-k))
