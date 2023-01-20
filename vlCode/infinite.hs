import Prelude hiding (take, repeat, iterate)

-- The infinite list of all numbers starting from a given one:
-- from 0 --> 0 : 1 : 2 : 3 : ....
from :: Int -> [Int]
from n = n : from (n+1)

-- Take the first n elements of some list:
take :: Int -> [a] -> [a]
take n xs | n <= 0 = []
take n []          = []
take n (x:xs)      = x : take (n - 1) xs


-- Sieve of Eratosthenes:
sieve :: [Int] -> [Int]
sieve (p : xs) = p : sieve (filter (\x -> x `mod` p > 0) xs)

-- All prime numbers:
primes :: [Int]
primes = sieve (from 2)

-- Returns the n-th prime numbers:
prim :: Int -> Int
prim n = primes !! (n - 1)


-- List of all Fibonacci numbers:
fibs :: [Integer]
fibs = fibsgen 0 1
 where
  fibsgen :: Integer -> Integer -> [Integer]
  fibsgen n1 n2 = n1 : fibsgen n2 (n1  + n2)

fib :: Int -> Integer
fib n = fibs !! n


-- Computes the infinite list of some value:
repeat :: a -> [a]
repeat x = x : repeat x

-- Iterated application of some function:
-- iterate f x --> x : f x : f (f x) : ...
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

nats = iterate (+1) 0

powers2 = iterate (*2) 1

-- fromThen n1 n2 --> [n1, n2, n2 + (n2-n1),...]
fromThen :: Int -> Int -> [Int]
fromThen n1 n2 = let d = n2 - n1 in n1 : fromThen (n1+d) (n2+d)

-- [n ... m]
fromTo :: Int -> Int -> [Int]
--fromTo n m = take (m - n + 1) (from n)
fromTo n m = if n>m then [] else n : fromTo (n + 1) m

fromThenTo :: Int -> Int -> Int -> [Int]
fromThenTo n1 n2 m = 
 let d = n2 - n1
 in if (d>=0 && n1>m) || (d<0 && n1<m)
      then []
      else n1 : fromThenTo (n1 + d) (n2 + d) m

-- Factorial function:
fac :: Int -> Int
fac n = foldr (*) 1 [1 .. n]

data Color = Red | Blue | Green
 deriving (Show, Enum, Bounded)

{-
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]  -- [n..]
  enumFromThen :: a -> a -> [a] -- [n1,n2..]
  enumFromTo   :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  ...
  enumFrom x = x : enumFrom (succ x)

class Bounded a where
 minBound, maxBound :: a
-}

-- An infinite list of numbered lines:
numLines :: [String]
numLines = map (uncurry (++)) (zip (map show [1..]) (repeat ". Zeile"))
