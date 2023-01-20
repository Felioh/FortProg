allCombinations :: [a] -> [[a]]
allCombinations list = let  generate tail = generatels tail list ++ generate (generatels tail list)
                            generatels tail [] = []
                            generatels tail (a : baselist) = map (a :) tail ++ generatels tail baselist
                      in [] : generate [[]]