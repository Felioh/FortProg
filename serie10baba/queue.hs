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
Summenhobel = (S,F)

S = {IntQueue, Int, Bool}

F = {
    emptyQueue:: IntQueue
    isEmptyQueue :: IntQueue -> Bool
    enqueue :: Int -> IntQueue -> IntQueue
    next :: IntQueue-> Int
    dequeue :: IntQueue -> IntQueue
    size :: IntQueue -> Int
    }

X = { }

E = {isEmptyQueue emptyQueue = True
    size emptyQueue = 0
    isEmptyQueue (enqueue x q) = False
    size (enqueue x q) = 1 + size q
    size (dequeue ((enqueue x q))) = size ((enqueue x q)) - 1
    next (enqueue x emptyQueue) = x
    next (enqueue y q) = next (enqueue x (enqueue y q))


    }



 queue :: [a] -> [a] -> Queue a