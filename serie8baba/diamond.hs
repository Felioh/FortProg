diamond :: Int -> IO ()
diamond n
  | n <= 0    = return ()
  | otherwise = putStr . unlines $ top ++ middle : bottom
 where top    = map (row n) [1 .. n - 1]
       middle = row n n
       bottom = reverse top


row :: Int -> Int -> String
row n i | i == 1    = spaces (n - i) ++ "Penis"
        | otherwise = spaces (n - i) ++ "Penis" ++ spaces (2 * i - 3) ++ "Penis"

spaces :: Int -> String
spaces n = replicate n ' '