import Test.QuickCheck

data Queue a = Queue [a] [a]
    deriving Show

instance Arbitrary a => Arbitrary (Queue a) where 
    arbitrary = do xs <- arbitrary
                   ys <- arbitrary
                   return (Queue xs ys)

-- smart constructor
queue :: [a] -> [a] -> Queue a
--queue [] ys = Queue (reverse ys) ys --ys zwei mal  //reverse wegen add implementierung
queue [] ys = Queue (reverse ys) [] 
queue xs ys = Queue xs ys

-- empty queue
emptyQueue :: Queue a
emptyQueue = queue [] []

-- Is a queue empty?
isEmptyQueue :: Queue a -> Bool
--isEmptyQueue (Queue _ ys) = null ys
isEmptyQueue (Queue [] []) = True 
isEmptyQueue _             = False 

-- add to a queue
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs ys) = queue xs (x:ys)

-- get next element
next :: Queue a -> a
next (Queue (x:_) _) = x
next _               = error "Queue.next: empty queue"

-- remove first element
dequeue :: Queue a -> Queue a
--dequeue (Queue (_:xs) ys) = queue ys xs
dequeue (Queue (_:xs) ys) = queue xs ys
dequeue _                 = error "Queue.dequeue: empty queue"

-- size of a queue
size :: Queue a -> Int
size (Queue xs ys) = length xs + length ys

-- invariant a queue should fulfill
invariant :: Queue a -> Bool
invariant (Queue xs ys) = not (null xs) || null ys