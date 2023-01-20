map':: (a -> b) -> [a] -> [b]
map' f list = foldr ((:).f) [] list

--listo = [(1,e),(2,d),(3,c),(4,b),(5,a)]

reverse':: [a] -> [a]
reverse' = foldr (\x xs -> xs ++ [x]) []

unzip':: [(a, b)] -> ([a], [b])
unzip' = foldr (\(x,y) (xs,ys) -> (x:xs, y:ys)) ([],[])


nub:: [Int] -> [Int]
--nub = foldr (\x xs -> x: filter (x/=) xs) []
nub = foldl (\xs x -> filter (x/=) xs ++ [x]) []

