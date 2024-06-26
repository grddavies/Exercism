module SumOfMultiples (sumOfMultiples) where

import Data.Function (on)
import Data.List (foldl', groupBy, sortBy, subsequences, tails)
import Data.List.NonEmpty (nonEmpty)
import Data.Ord (comparing)

-- | Calculates the sum of the multiples of the factors up to a given limit
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ zipWith ($) (cycle [sum, negate . sum]) sums
  where
    sums = map (fmap (sumBelowLim limit . lcmList)) (sublists factors)

-- | Calculates the sum of the arithmetic sequence
sumArith :: (Integral a) => a -> a -> a -> a
sumArith a d n = n * (2 * a + (n - 1) * d) `div` 2

-- | Calculates the sum of multiples of a number up to a given limit
sumBelowLim :: (Integral a) => a -> a -> a
sumBelowLim lim n
    | n == 0 = 0
    | otherwise = sumArith n n ((lim - 1) `div` n)

-- | Calculates the least common multiple of a list of numbers
lcmList :: (Integral a) => [a] -> a
lcmList = foldr lcm 1

-- | Creates a list of lists of nonempty subsets of a list with the same length
sublists :: [a] -> [[[a]]]
sublists xs = groupBy ((==) `on` length) $ sortBy (comparing length) (nonEmptySubseqs xs)
  where
    nonEmptySubseqs = filter (not . null) . subsequences