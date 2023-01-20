import Data.Maybe (listToMaybe)

--[[3],[5],[7],[9],[11]]
list1 :: [[Integer]]
list1 = [ [(i*2)+1] | i <- [1 .. 5]]


--[(5,False),(20,True),(25,False)]
list2 :: [(Integer, Bool)]
list2 = [ (5*i,even i) | i <- [1 .. 5], i==1|| i>3 ]

--[Just 1,Just 9,Just 25]
list3 :: [Maybe Integer]
list3 = [ Just (i^2) | i <- [1 .. 5], odd i ]

--[(1,5),(1,4),(1,3),(1,2),(2,5),(2,4),(2,3),(3,5),(3,4),(4,5)]
list4 :: [(Integer, Integer)]
list4 = [ (i,j) | i <- [1 .. 5], j <- [5, 4 .. i + 1]]

list :: [Integer]
list = [1,2,3,4,5]

map':: (a->b) -> [a] -> [b]
map' f list = [ f i | i <- list]

lookup':: Eq a => a -> [(a,b)] -> Maybe b
lookup' x list = listToMaybe  [b | (a,b) <- list, x == a]

replicate':: Int -> a -> [a]
replicate' n a = [a| i <- [0 .. n]]

filter':: (a -> Bool) -> [a] -> [a]
filter' f list = [a | a <- list , f a] 


{- 
map' f list = [ f i | i <- list]

lookup':: Eq a => a -> [(a, b)] -> Maybe b
lookup' a list = listToMaybe [ j | (i,j) <- list, i == a]

replicate':: Int -> a -> [a]
replicate' n x = [ x | _ <- [1 .. n]]


filter' f list = [ i | i <- list , f i]

test :: Maybe Int
test = lookup' 7 (map' (\x -> (x, 6 * x)) (filter' odd (replicate' 4 7))) -}