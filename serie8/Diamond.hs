

diamond :: Int -> IO ()
diamond n = build 1 n
  where build n max = if n <= max then putStrLn (take (max-n) (repeat ' ') ++ take n (repeat '*')) >> build(n+1) max
                                  else putStrLn "*"