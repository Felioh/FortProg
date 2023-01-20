import Prelude hiding ( print, putStr, putStrLn, getLine )
{-
-- Unit type:
data () = ()

main :: ()
main = let str = getLine
       in putStr str

main1 = let x = putStr "Hi"
        in x ; x

main2 = putStr "Hi" ; putStr "Hi"

main3 = let base = readInt
            exp  = readInt
        in foldr (*) 1 (map (const base) [1 .. exp])

Konsequenz: Seiteneffekt haben ueberraschende Effekte!
-}

main1 :: IO ()
main1 = putStr "Hi"

main2 = putStr "Hi" >> putStr "Hi"

main3 = let x = putStr "Hi"
        in x >> x

dupAct :: IO () -> IO ()
dupAct x = x >> x

main4 = dupAct (putStr "Hi")

main5 = let acts = repeat (putStr "Hi")
        in (acts!!42) >> (acts!!100)

-- Executes n times a given action (n>0):
multAct :: Int -> IO () -> IO ()
multAct n act = foldr1 (>>) (take n (repeat act))

main6 = multAct 2 (putStr "Hi")

santa_claus = multAct 3 (putStr "Ho ")

fac :: Int -> Int
fac n = if n==0 then 1 else n * fac (n-1)

main7 = putStr (show (fac 10))

main8 = print (fac 10)

print :: Show a => a -> IO ()
print x = putStr (show x) >> putChar '\n'

putStr :: String -> IO ()
putStr []     = return () -- "do nothing"
putStr (c:cs) = putChar c >> putStr cs

putStrLn :: String -> IO ()
putStrLn s = putStr s >> putChar '\n'

getLine0 :: IO String
getLine0 =
  getChar >>= \c -> if c == '\n'
                      then return ""
                      else getLine0 >>= \s -> return (c:s)

getLine :: IO String
getLine =
  do c <- getChar
     if c == '\n'
       then return ""
       else do s <- getLine
               return (c:s)

-- Computes the factorial with tracing intermediate results:
traceFac :: Int -> IO Int
traceFac n =
  if n==0 then return 1
          else do f <- traceFac (n - 1)
                  print (n-1, f)
                  return (n * f)

-- Our main interactive program:
mainFac :: IO ()
mainFac = do
  putStr "Enter a number: "
  str <- getLine
  --f <- traceFac (read str)
  let f = fac (read str)
  putStrLn ("The factorial is " ++ show f)
