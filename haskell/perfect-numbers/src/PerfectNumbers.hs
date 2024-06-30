module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n < 1 = Nothing
  | n < aliquotSum = Just Abundant
  | n == aliquotSum = Just Perfect
  | n > aliquotSum = Just Deficient
  where
    aliquotSum = sum $ factors n

    factors :: Int -> [Int]
    factors x = [y | y <- [1.. x `div` 2], x `mod` y == 0]

