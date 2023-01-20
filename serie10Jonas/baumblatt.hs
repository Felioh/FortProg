import Test.QuickCheck

data Tree = Leaf Int | Node Tree Tree
  deriving (Eq, Show)

instance Arbitrary Tree where
    arbitrary = do x <- arbitrary 
                   l <- arbitrary 
                   r <- arbitrary 
                   oneof [return (Leaf x), return (Node l r)]

size :: Tree -> Int
size (Leaf x)   = 1
size (Node l r) = size l + size r 

toList :: Tree -> [Int]
toList (Leaf x)   = [x]
toList (Node l r) = toList l ++ toList r

flatten:: Tree -> [Int]
flatten (Leaf x)   = [x]
flatten (Node l r) = flatten l ++ flatten r

main:: IO ()
main = do putStrLn "prop_flatten_size:  "
          quickCheck prop_flatten_size

prop_flatten_size:: Tree -> Bool 
prop_flatten_size tree = size tree == length (toList tree)