import SimplePrelude

intSum1 :: Int -> Int
intSum1 n = div (n * (n + 1)) 2

intSum2 :: Int -> Int
intSum2 n = if n == 0 then 0 else n + intSum2 (n - 1)

intSum3 :: Int -> Int
intSum3 n = intSum3' n 0
  where
    intSum3' :: Int -> Int -> Int
    intSum3' i sum = if i == 0 then sum else intSum3' (i - 1) (sum + i)

{-
int sum = 0;
for (int i = n; i > 0; i--) {
  sum += i;
}
return sum;
-}

intSum4 :: Int -> Int
intSum4 n = intSum4' 0 0
  where
    intSum4' :: Int -> Int -> Int
    intSum4' i sum = if i <= n then intSum4' (i + 1) (sum + i) else sum

{-
int sum = 0;
for (int i = 0; i <= n; i++) {
  sum += i;
}
return sum;
-}
