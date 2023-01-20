-- Computes an element in the pascal triangle
-- e.g. pascal 4 2 will return 6

pascal :: Int -> Int -> Int
pascal row pos = if pos == 0 || row == pos then 1 
                 else pascal (row-1) (pos-1) + pascal (row-1) pos
