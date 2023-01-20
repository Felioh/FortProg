--Die Funktion insert :: a -> [a] -> [[a]] soll ein gegebenes Element an jeder Stelle einer Liste einfügen.
--Zum Beispiel soll der Aufruf insert 1 [2,3] die Liste [[1,2,3],[2,1,3],[2,3,1]] liefern (oder eine Liste, die dieselben Listen in anderer Reihenfolge enthält).
insert :: a -> [a] -> [[a]]
insert elem list = add elem [] list
        


add:: a -> [a] -> [a] -> [[a]]
add elem preList []     = [preList++[elem]]
add elem preList (x:xs) = (preList ++ [elem] ++ (x:xs)) : add elem (preList++[x]) xs


--Die Funktion perms :: [a] -> [[a]] soll alle Permutationen einer Liste berechnen.
--perms :: [a] -> [[a]]
--perms (y:x:xs) = add2 y [] (x:xs) ++ add2 x [] (y:xs)

add2:: a -> [a] -> [a] -> [[a]]
add2 elem preList []     = [preList++[elem]]
add2 elem preList (x:xs) = (preList ++ [elem] ++ (x:xs)) : add2 elem (preList++[x]) xs



--Die Funktion insert kann man verwenden, um alle Permutationen einer Liste zu berechnen. 
--Dazu fügt man das erste Element einer nicht-leeren Liste an einer beliebigen Stelle in jede Permutation der Restliste ein.

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = permInsert [] (perms xs)
  where
    permInsert acc []       = acc
    permInsert acc (ys:yss) = permInsert (acc ++ insert x ys) yss
