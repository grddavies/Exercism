module Triangle (rows) where

-- Generate row n of Pascal's triangle
row :: Int -> [Int]
row 0 = []
row n = zipWith (+) (0:xs) xs ++ [1] where
    xs = row (n - 1)

-- Generate first n rows of Pascal's triangle
rows :: Int -> [[Int]]
rows 0 = []
rows n = rows (n-1) ++ [row n]

