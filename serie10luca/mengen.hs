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
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

data Set a = Set [a]
  deriving Show

instance Arbitrary a => Arbitrary (Set a) where
  arbitrary = do
    xs <- arbitrary
    return (Set xs)

-- aus ML. Zu aufwändig. Zu lang. Lesen reicht.

prop_isEmpty_empty :: Bool
prop_isEmpty_empty = isEmpty empty

prop_member_empty :: Int -> Bool
prop_member_empty x = not (member x empty)

prop_size_empty :: Bool
prop_size_empty = size empty == 0

prop_isEmpty_insert :: Int -> Set Int -> Bool
prop_isEmpty_insert x s = not (isEmpty (insert x s))

prop_member_insert :: Int -> Set Int -> Bool
prop_member_insert x s = member x (insert x s)

prop_member_insert2 :: Int -> Int -> Set Int -> Property
prop_member_insert2 x y s = x /= y ==> member y (insert x s) == member y s

prop_size_insert :: Int -> Set Int -> Bool
prop_size_insert x s =
  size (insert x s) == size s + (if member x s then 0 else 1)

prop_isEmpty_delete :: Int -> Set Int -> Property
prop_isEmpty_delete x s = isEmpty s ==> isEmpty (delete x s)

prop_member_delete :: Int -> Set Int -> Bool
prop_member_delete x s = not (member x (delete x s))

prop_member_delete2 :: Int -> Int -> Set Int -> Property
prop_member_delete2 x y s = x /= y ==> member y (delete x s) == member y s

prop_size_delete :: Int -> Set Int -> Bool
prop_size_delete x s =
  size (delete x s) == size s - (if member x s then 1 else 0)

prop_isEmpty_union :: Set Int -> Set Int -> Bool
prop_isEmpty_union s1 s2 = isEmpty (union s1 s2) == (isEmpty s1 && isEmpty s2)

prop_member_union :: Int -> Set Int -> Set Int -> Bool
prop_member_union x s1 s2 =
  (member x s1 || member x s2) == member x (union s1 s2)

prop_size_union :: Set Int -> Set Int -> Bool
prop_size_union s1 s2 =
  size (union s1 s2) == size s1 + size s2 - size (intersect s1 s2)

prop_isEmpty_intersect :: Set Int -> Set Int -> Property
prop_isEmpty_intersect s1 s2 =
  isEmpty s1 || isEmpty s2 ==> isEmpty (intersect s1 s2)

prop_member_intersect :: Int -> Set Int -> Set Int -> Bool
prop_member_intersect x s1 s2 =
  (member x s1 && member x s2) == member x (intersect s1 s2)

prop_size_intersect :: Set Int -> Set Int -> Bool
prop_size_intersect s1 s2 = s >= 0 && s <= size s1 && s <= size s2
  where s = size (intersect s1 s2)

main :: IO ()
main = do
  putStr "prop_isEmpty_empty    : "
  quickCheck prop_isEmpty_empty
  putStr "prop_member_empty     : "
  quickCheck prop_member_empty
  putStr "prop_size_empty       : "
  quickCheck prop_size_empty
  putStr "prop_isEmpty_insert   : "
  quickCheck prop_isEmpty_insert
  putStr "prop_member_insert    : "
  quickCheck prop_member_insert
  putStr "prop_member_insert2   : "
  quickCheck prop_member_insert2
  putStr "prop_size_insert      : "
  quickCheck prop_size_insert
  putStr "prop_isEmpty_delete   : "
  quickCheck prop_isEmpty_delete
  putStr "prop_member_delete    : "
  quickCheck prop_member_delete
  putStr "prop_member_delete2   : "
  quickCheck prop_member_delete2
  putStr "prop_size_delete      : "
  quickCheck prop_size_delete
  putStr "prop_size_delete      : "
  quickCheck prop_size_delete
  putStr "prop_isEmpty_union    : "
  quickCheck prop_isEmpty_union
  putStr "prop_member_union     : "
  quickCheck prop_member_union
  putStr "prop_size_union       : "
  quickCheck prop_size_union
  putStr "prop_isEmpty_intersect: "
  quickCheck prop_isEmpty_intersect
  putStr "prop_member_intersect : "
  quickCheck prop_member_intersect
  putStr "prop_size_intersect   : "
  quickCheck prop_size_intersect


-- alternativ:
return []
testAll = $quickCheckAll
