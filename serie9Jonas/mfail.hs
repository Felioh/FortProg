module Eval where

import MFail

data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
  deriving Show

eval :: MFail m => Exp -> m Int
eval (Num     n) = pure n
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (Sub e1 e2) = (-) <$> eval e1 <*> eval e2
eval (Mul e1 e2) = (*) <$> eval e1 <*> eval e2
eval (Div e1 e2) = do x <- eval e1
                      y <- eval e2
                      safediv x y
  where
    safediv x y | y == 0    = mfail "Division by zero"
                | otherwise = Just (x `div` y)

e1 :: Exp
e1 = Add (Num 3) (Mul (Num 4) (Num 2))

e2 :: Exp
e2 = Add (Num 3) (Div (Num 4) (Sub (Num 2) (Num 2)))

class (Monad m) => MFail m where
  mfail :: String -> m a

instance MFail Maybe where
    mfail _ = Nothing 

instance MFail [] where
  -- mfail :: String -> [a]
    mfail _ = []
  
instance MFail Error where
  -- mfail :: String -> Error a
    mfail s = Failure s