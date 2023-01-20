
import Data.Ratio ((%))
fibonacci :: [Integer]                                                      --Optional: Aus der Vorlesung
fibonacci = build 0 1
  where
      build fst snd = fst : build snd (fst + snd)

goldenRatio :: [Rational]
goldenRatio = build 0 1  
  where
      build fst snd = (1 + (fst % snd)) : build snd (fst+snd)


approx :: Rational -> [Rational] -> Rational
approx eps (x : xs) = go x xs
  where 
      go elem (snd : ls) = if abs (elem - snd) <= eps then snd
                            else go snd ls


