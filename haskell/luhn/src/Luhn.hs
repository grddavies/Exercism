module Luhn (isValid) where

import Data.Char (digitToInt, isDigit, isSpace)

isValid :: String -> Bool
isValid str
  | length stripped <= 1 = False
  | not (all isDigit stripped) = False
  | otherwise = sum doubled `mod` 10 == 0
 where
  stripped = filter (not . isSpace) str
  doubled = zipWith ($) (cycle [id, double]) $ map digitToInt $ reverse stripped
  double x = let d = x * 2 in if d > 9 then d - 9 else d