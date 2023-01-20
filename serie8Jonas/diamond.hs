diamond :: Int -> IO ()
diamond n = do  let string = linecalc n 0
                    in do putStr string




linecalc:: Int -> Int -> String
linecalc 0 pos = " " ++ "\n"
linecalc 1 pos =  "*" ++ "\n"
linecalc n pos = lineup (n-1) (pos+1)  ++  "*" ++ replicate n ' ' ++"*"++  linedown (n-1) (pos+1)


linedown :: Int -> Int -> String
linedown 1 pos = "\n" ++ replicate pos ' ' ++ "*" ++"\n"
linedown n pos = "\n" ++ replicate pos ' '  ++ "*" ++ replicate (n-1) ' ' ++"*"++ linedown (n-1) (pos+1)


lineup :: Int -> Int -> String
lineup 0 pos = " " ++ "\n"
lineup 1 pos = lineup 0 (pos+1) ++ replicate pos ' ' ++ "*"++ "\n"
lineup n pos = lineup (n-1) (pos+1) ++   replicate (pos-1) ' ' ++ "*" ++ replicate (n-1) ' ' ++"*"++ "\n"