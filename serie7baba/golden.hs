import Data.Ratio

fibonacci :: [Integer]
fibonacci = calc 0 0
            where calc 0    0     = 1 : calc 0 1
                  calc fibn fibn1 = (fibn + fibn1): calc fibn1 (fibn + fibn1)



goldenRatio :: [Rational]
goldenRatio = calc 0
            where calc 0 = (1 % last(take 1 fibonacci)):calc (0+1)
                  calc n = (1 + (last(take n fibonacci) % last(take (n+1) fibonacci))):calc (n+1)


approx :: Rational -> [Rational] -> Rational
approx eps xs = calc eps xs
            where calc eps (x:y:ys) = if  abs(x - y) <= eps then y
                                                                    else calc eps (y:ys)
                        