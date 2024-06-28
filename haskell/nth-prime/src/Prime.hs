module Prime (nth) where

nth :: Int -> Maybe Integer
nth n = if n < 1 then Nothing else Just (primes !! (n - 1))
 where
  primes = filter isPrime [2 ..]

isPrime :: Integer -> Bool
isPrime n = (n > 1) && loop 0 wheel
 where
  basis = [2, 3, 5]
  prod = product basis
  wheel = basis ++ filter (\x -> all (\y -> x `mod` y /= 0) basis) [2 .. prod + 1]

  loop :: Integer -> [Integer] -> Bool
  loop base [] = loop (base + prod) wheel
  loop base (x : xs) =
    let i = base + x
     in i * i > n || n `mod` i /= 0 && loop base xs
