avalanche :: [Integer]
avalanche = filter isAv [1..]
    where isAv 1                = True
          isAv x | mod x 7 == 0 = isAv (div x 7)
          isAv x | mod x 5 == 0 = isAv (div x 4)
          isAv x | mod x 3 == 0 = isAv (div x 3)
          isAv _                = False