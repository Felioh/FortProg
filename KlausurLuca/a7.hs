data BExp = Const Bool | Var String | And BExp BExp | Or BExp BExp | Not BExp

vars :: BExp -> [String]
vars b = go b []
    where go (Var v) ac     = ac ++ [v]
          go (And b1 b2) ac = go b1 ac ++ go b2 ac
          go (Or b1 b2) ac  = go b1 ac ++ go b2 ac
          go (Not b1 ) ac   = go b1 ac


type Valuation = [(String, Bool)]

test = Or (Const True) (Const False)

value :: Valuation -> BExp -> Bool
value vs (Const c) = c -- straight
value vs (And a b) = value vs a && value vs b -- A && B
value vs (Or  a b) = value vs a || value vs b -- A || B
value vs (Not a  ) = not (value vs a)         -- not A
value vs (Var v  ) = case lookup v vs of      -- Var need to be looked up in "Valuation"
                        (Just c) -> c
                        _        -> False
    
    
    
    
  



          
