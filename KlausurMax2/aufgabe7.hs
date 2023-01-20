data Number = Num [()]
  deriving (Show, Eq)

instance Ord Number where               
    compare x y = compare (fromNumber x) (fromNumber y)


fromNumber :: Number -> Int        
fromNumber x = count x 0
    where count (Num (x : xs)) c = count (Num xs) (c + 1)
          count (Num []) c       = c

toNumber :: Int -> Maybe Number       
toNumber x = if x >= 0 then Just (Num (count x)) else Nothing
    where count 0 = []
          count a = () : count (x-1)