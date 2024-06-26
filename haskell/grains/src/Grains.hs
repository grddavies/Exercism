module Grains (square, total) where

import Data.Maybe (fromJust)

-- | Calculate the number of grains on a given chessboard square
square :: Integer -> Maybe Integer
square n
  | n < 1 = Nothing
  | n > 64 = Nothing
  | otherwise = Just (2 ^ (n - 1))

-- | Calculate the total grains chessboard
total :: Integer
total = sum (fromJust (mapM square [1 .. 64]))
