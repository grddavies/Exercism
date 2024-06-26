module CollatzConjecture (collatz) where

import Data.List (unfoldr)

collatz :: Integer -> Maybe Integer
collatz n
  | n > 0 = Just $ countSteps n
  | otherwise = Nothing
 where
  countSteps = toInteger . length . unfoldr nextStep

  nextStep 1 = Nothing
  nextStep k = Just (k, if even k then k `div` 2 else 3 * k + 1)
