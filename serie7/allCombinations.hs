
allCombinations :: [a] -> [[a]]
allCombinations ls = [] : addAll [[]]
  where
      addAll lastls = genls lastls ls ++ addAll (genls lastls ls) --let in better
      genls lastls [] = []
      genls lastls (a : basels) = map (a :) lastls ++ genls lastls basels