import Prelude hiding (concat)

-- List comprehension:

odds :: [Int]
odds = [ x | x <- [0..], odd x ]

list1 = [ (x,y) | x <- [1 .. 4], y <- [2 .. 6], x /= y ]

list2 = [ x+y | x <- [1 .. 4], y <- [2 .. 6], x /= y ]

list3 = [ (x,y,z) | x <- [1 .. 4], y <- [2 .. 6], let z = x+y, x /= y ]

-- all prefixes if natural numbers:
allPrefixes = [ [0 .. n] | n <- [0 ..] ]

concat :: [[a]] -> [a]
concat xss = [ y | ys <- xss, y <- ys ]
