import SimplePrelude

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []     -- ähnlich mit foldl möglich, dann müsste man allerdings flippen und die liste danach umdrehen.


reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) [] -- foldl hier wichtig, um die umgekehrte Reihenfolge zu erhalten, da das nächste element immer vorne angefügt wird


unzip' :: [(a, b)] -> ([a], [b])
unzip' ls = (getls fst, getls snd)
  where getls f = foldr ((:) . f) [] ls --foldr, damit die reihenfolde der Elemente beibehalten wird
--statt fst : \(a, b) -> a    statt snd : \(a, b) -> b
--unzip' ls = (foldl (flip ((:) . fst)) [] ls, foldl (flip ((:) . snd)) [] ls)


nub' :: [Int] -> [Int]
nub'  = foldr (\x -> (x : ) . filter (/=x)) [] --foldr weil es einfacher ist, aber theoretisch auch mit foldl möglich
