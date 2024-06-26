module Luhn (isValid) where

import Data.Char (isDigit, isSpace)

isValid :: String -> Bool
isValid str =
  (length stripped > 1) && all isDigit stripped && sum doubled `mod` 10 == 0
 where
  stripped = filter (not . isSpace) str
  doubled = zipWith (\i x -> if odd i then (doubleSub . readChar) x else readChar x) [0 ..] (reverse stripped)
  doubleSub x = let d = x * 2 in if d > 9 then d - 9 else d
  readChar x = read [x]