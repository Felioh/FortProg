avalance :: [Integer]
avalance = filter aval (nums 1)
  where nums n = n : nums (n + 1)
        aval n | n == 1 = True
               | mod n 7 == 0 = aval (div n 7)
               | mod n 5 == 0 = aval (div n 5)
               | mod n 3 == 0 = aval (div n 3)
               | otherwise = False
