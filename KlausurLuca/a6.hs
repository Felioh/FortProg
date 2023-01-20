-- 1. 
-- data X t = One t t | Two t


-- 3.
data BExp = Const Bool | Var String | And BExp BExp | Or BExp BExp | Not BExp
    deriving Show

test = Or (Var "A") (Var "B")

normalize :: BExp -> BExp
normalize (Not b) = go b
    where 
        go (Or x y)  = And (Not x) (Not y)
        go (And x y) = Or (Not x) (Not y)
        go (Not x)   = x



-- 4.

-- type Valuation = [(String, Bool)]

-- allValuations :: [String] -> [Valuation]
-- allValuations ls = [] : go [[]]
--     where go res 




