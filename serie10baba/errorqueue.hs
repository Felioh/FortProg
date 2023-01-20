import Test.QuickCheck

data Queue a = Queue [a] [a]
    deriving Show

-- smart constructor
queue :: [a] -> [a] -> Queue a
queue [] ys = Queue (reverse ys) ys
queue xs ys = Queue xs ys

-- empty queue
emptyQueue :: Queue a
emptyQueue = queue [] []

-- Is a queue empty?
isEmptyQueue :: Queue a -> Bool
isEmptyQueue (Queue _ ys) = null ys

-- add to a queue
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs ys) = queue xs (x:ys)

-- get next element
next :: Queue a -> a
next (Queue (x:_) _) = x
next _               = error "Queue.next: empty queue"

-- remove first element
dequeue :: Queue a -> Queue a
dequeue (Queue (_:xs) ys) = queue ys xs
dequeue _                 = error "Queue.dequeue: empty queue"

-- size of a queue
size :: Queue a -> Int
size (Queue xs ys) = length xs + length ys

-- invariant a queue should fulfill
invariant :: Queue a -> Bool
invariant (Queue xs ys) = not (null xs) || null ys 
{- (not (A) || B ) == (A => B) -}

instance Arbitrary a => Arbitrary (Queue a) where
    arbitrary = do xs <- arbitrary
                   ys <- arbitrary
                   return  (Queue xs ys)

prop_queue :: [Int] -> [Int] -> Bool
prop_queue xs ys = invariant (queue xs ys)

prop_size_queue :: [Int] -> [Int] -> Bool
prop_size_queue xs ys = length xs + length ys == size (queue xs ys)

prop_emptyQueue :: Bool
prop_emptyQueue = invariant emptyQueue

{- 
    isEmpty empty = True
    , member x empty = False
    , member x (insert s x) = True
    , member x (insert s y) = member x s, falls x /= y
    , union empty empty = empty
    , union (insert empty x) s = insert s x
    , isEmpty (union s t) = isEmpty s && isEmpty t
    , member x (union s t) = member x s || member x t
    , isEmpty (insert s x) = False -}