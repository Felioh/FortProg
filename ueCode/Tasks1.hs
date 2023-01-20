

-- Wiederholung:

{-elemBool :: Bool -> [Bool] -> Bool
elemBool x []     = False
elemBool x (y:ys) = if eqBool x y then True else elemBool x ys

eqBool :: Bool -> Bool -> Bool
eqBool False False = True
eqBool True  True  = True
eqBool _     _     = False

elemInt :: Int -> [Int] -> Bool
elemInt x []     = False
elemInt x (y:ys) = if eqInt x y then True else elemInt x ys

eqInt :: Bool -> Bool -> Bool
eqInt n1 n2 = -- Hier prüfen ob n1 und n2 gleich sind.
eqInt 0 0 = True
eqInt 1 1 = True
-- ...-}

elem' :: Eq a => a -> [a] -> Bool
elem' x []     = False
elem' x (y:ys) = if x == y then True else elem' x ys

{-class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool-}

{-instance Eq JSON where
  -- (==) :: JSON -> JSON -> Bool
  JNull      == JNull      = True
  JBool b1   == JBool b2   = b1 == b2
  JInt i1    == JInt i2    = i1 == i2
  JFloat f1  == JFloat f2  = f1 == f2
  JString s1 == JString s2 = s1 == s2
  JArray l1  == JArray l2  = l1 == l2
  JObject l1 == JObject l2 = l1 == l2
  _          == _          = False
  -- (/=) :: JSON -> JSON -> Bool
  x /= y = not (x == y)-}


{-instance Eq a => Eq [a] where
  []     == []     = True
  (x:xs) == (y:ys) = x == y && xs == ys
  _      == _      = False-}

{-data Tree a = Empty | Node (Tree a) a (Tree a)

instance Eq a => Eq (Tree a) where
  -- (==) :: Tree a -> Tree a -> Bool
  Empty           == Empty           = True
  Node t1l n1 t1r == Node t2l n2 t2r =
    n1 == n2 && t1l == t2l && t1r == t2r
  _               == _               = False
-}



{-class Eq a => Ord a where
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  --...-}

{-length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs-}

data JSON = JNull
          | JBool Bool
          | JInt Int
          | JFloat Float
          | JString String
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving Show

class ToJSON a where
  toJSON :: a -> JSON

-- Allgemein:
-- instance [Kontext =>] <Typklassenname> <Instanztyp> where
--   <Definitionen für Klassenmethoden>
instance ToJSON Bool where
  -- toJSON :: Bool -> JSON
  {-toJSON False = JBool False
  toJSON True  = JBool True-}
  toJSON b = JBool b

instance ToJSON Int where
  -- toJSON :: Int -> JSON
  toJSON i = JInt i

instance ToJSON a => ToJSON [a] where
  -- toJSON :: [a] -> JSON
  toJSON as = JArray (map toJSON as)
  {-toJSON []     = JArray []
    toJSON (x:xs) = case toJSON xs of
                      JArray jsons -> JArray (toJSON x : jsons) -}

instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
  -- toJSON :: (a, b) -> JSON
  toJSON (x, y) = JObject [("fst", toJSON x), ("snd", toJSON y)]
  --toJSON (x, y) = JArray [toJSON x, toJSON y]

data Tree a = Empty | Node (Tree a) a (Tree a)

instance ToJSON a => ToJSON (Tree a) where
  -- toJSON :: Tree a -> JSON
  toJSON Empty          = JNull
  toJSON (Node tl n tr) =
    JObject [field "leftTree" tl, field "node" n, field "rightTree" tr]
    where field name x = (name, toJSON x)
  --toJSON (Node tl n tr) = JArray [toJSON tl, toJSON n, toJSON tr]

foldJSON :: a                    -- ^ jnull
         -> (Bool -> a)          -- ^ jbool
         -> (Int -> a)           -- ^ jint
         -> (Float -> a)         -- ^ jfloat
         -> (String -> a)        -- ^ jstring
         -> ([a] -> a)           -- ^ jarray
         -> ([(String, a)] -> a) -- ^ jobject
         -> JSON                 -- ^ JSON document to be folded
         -> a                    -- ^ result of folding a JSON value
foldJSON jnull jbool jint jfloat jstring jarray jobject doc = case doc of
  JNull     -> jnull
  JBool   b -> jbool b
  JInt    i -> jint i
  JFloat  f -> jfloat f
  JString s -> jstring s
  JArray  a -> jarray (map (foldJSON jnull jbool jint jfloat jstring jarray jobject) a)
  JObject o -> jobject (map help o)
    where
    --help :: (String, JSON) -> (String, a)
    help (k, v) = (k, foldJSON jnull jbool jint jfloat jstring jarray jobject v)