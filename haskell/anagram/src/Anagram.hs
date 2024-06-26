module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [x | x <- xss, isAnagram xs x]
  where
    isAnagram xs ys = xs' /= ys' && sort xs' == sort ys'
      where
        xs' = map toLower xs
        ys' = map toLower ys
