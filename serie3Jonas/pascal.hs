binom :: Int -> Int -> Int
binom n k = div (fac n) (fac k * fac (n-k))
        where fac 0 = 1
              fac n = n * fac (n-1)


pascal :: Int -> Int -> Int
pascal _   0                = 1
pascal 0   _                = 1
pascal row pos | row == pos = 1
               | otherwise  = pascal (row-1) pos + pascal (row-1) (pos-1)