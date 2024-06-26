module Strain (keep, discard) where

import Prelude hiding (filter)

discard :: (a -> Bool) -> [a] -> [a]
discard _p [] = []
discard p (x : xs)
    | p x = discard p xs
    | otherwise = x : discard p xs

keep :: (a -> Bool) -> [a] -> [a]
keep _p [] = []
keep p (x : xs)
    | p x = x : keep p xs
    | otherwise = keep p xs
