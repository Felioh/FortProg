
binom :: Int -> Int -> Int 
binom n k = div (fac n) ((fac k) * (fac (n-k))) 
    where fac x = if x == 0  then 1 else x * fac (x-1)

pascal :: Int -> Int -> Int
pascal row pos | pos == 0 = 1
               | row == 0 = 1
               | row == pos = 1
               | otherwise = pascal (row-1) pos + pascal (row-1) (pos-1)