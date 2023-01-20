allNumbers::[Integer]
allNumbers = calc 0
        where calc n = n: calc (n+1)


avalanche :: [Integer]
avalanche = filter funktion allNumbers
        where --funktion = \y -> (y `mod` 3 == 0 && y `mod` 5 == 0 && y `mod` 7 == 0) || y `mod` 3 == 0 || y `mod` 5 == 0 || y `mod` 7 == 0
              funktion = \y i j k -> 3^i * 5^j * 7^k == y

avalancheNaive :: [Integer]
avalancheNaive = filter isAvalanche [1 ..]
  where isAvalanche 1 = True
        isAvalanche n | n `mod` 7 == 0 = isAvalanche (n `div` 7)
        isAvalanche n | n `mod` 5 == 0 = isAvalanche (n `div` 5)
        isAvalanche n | n `mod` 3 == 0 = isAvalanche (n `div` 3)
        isAvalanche _ = False 