module Hamming (distance) where

-- Compute hamming distance between two String
-- representations of a DNA sequence
distance :: String -> String -> Maybe Int
distance xs ys = go xs ys 0
  where
    go [] [] d = Just d
    go xs [] _ = Nothing
    go [] ys _ = Nothing
    go (x : xs) (y : ys) d = go xs ys (if x == y then d else d + 1)
