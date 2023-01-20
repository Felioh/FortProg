
fak:: Int -> Int 
fak n = if n == 0 then 1 else n * fak (n-1)



binom :: Int -> Int -> Int
binom n k = div (fak n) (fak k * fak (n-k))

pascal :: Int -> Int -> Int
pascal row pos | row == 0 && pos==0 = 1
               | row == pos         = 1 
               | pos == 0           = 1  
               | otherwise          = pascal (row-1) pos + pascal (row-1) (pos-1)
