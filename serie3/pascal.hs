import SimplePrelude

-- Computes a selected number in the "Pascalsche Dreieck"
pascal :: Int -> Int -> Int
pascal row pos = if pos == 0 || pos == row then 1 else pascal (row-1) pos + pascal (row-1) (pos-1)