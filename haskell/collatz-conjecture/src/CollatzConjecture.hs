module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = go 0 n
 where
  go c x
    | x < 1 = Nothing
    | x == 1 = Just c
    | x `mod` 2 == 0 = go (c+1) (x `div` 2)
    | otherwise = go (c+1) (3 * x + 1)
