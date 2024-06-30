module PerfectNumbers (classify, Classification (..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n =
  if n < 1
    then Nothing
    else case compare n aliquotSum of
      LT -> Just Abundant
      EQ -> Just Perfect
      GT -> Just Deficient
 where
  aliquotSum = sum $ factors n

  factors :: Int -> [Int]
  factors x = [y | y <- [1 .. x `div` 2], x `mod` y == 0]
