{- ADT für Int-Mengen: (Sigma, X, E)

Sigma = (S, F)

S = { IntSet, Int, Bool }
F = { empty :: IntSet
    , isEmpty :: IntSet -> Bool
    , member :: Int IntSet -> Bool
    , insert :: IntSet Int -> IntSet
    , union :: IntSet IntSet -> IntSet
    }
X = { x :: Int, y :: Int, s :: IntSet, t :: IntSet }
E = { isEmpty empty = True
    , member x empty = False
    , member x (insert s x) = True
    , member x (insert s y) = member x s, falls x /= y
    , union empty empty = empty
    , union (insert empty x) s = insert s x
    , isEmpty (union s t) = isEmpty s && isEmpty t
    , member x (union s t) = member x s || member x t
    , isEmpty (insert s x) = False
    }
 -}


type IntSet = [Int]

empty :: IntSet
empty = []

member :: Int -> IntSet -> Bool
member _ []     = False
member x (y:ys) = x == y || member x ys
-- member = elem

isEmpty :: IntSet -> Bool
isEmpty [] = False
isEmpty _  = True
-- isEmpty = null

insert :: IntSet -> Int -> IntSet
insert xs x = x : xs


union :: IntSet -> IntSet -> IntSet
union = (++)

delete :: Int -> IntSet -> IntSet
delete _ []     = []
delete n (x : xs) | n == x    = delete n xs   -- wegen blöder insert implementierung :()
                  | otherwise = x : delete n xs

intersect :: IntSet -> IntSet -> IntSet
intersect (a:as) b = if member a b then a : intersect as b
                                   else intersect as b
size :: IntSet -> Int
size [] = 0
size (a:as)  = if member a as
               then size as
               else 1 + (size as)

size2 :: IntSet -> Int
size2 [] = 0
size2 (x:xs) = 1 + size2 ((filter (\y-> (y /= x)) xs))

size3 :: IntSet -> Int
size3 [] = 0
size3 list = len list 0
    where len (x:xs) n = len (delete x (x:xs)) (n+1)
          len [] n = n


