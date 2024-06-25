{-# OPTIONS_GHC -Wno-type-defaults #-}

module Darts (score) where

score :: Float -> Float -> Int
score x y
  | d > rO^1 = 0 -- miss
  | d > rM^2 = 1 -- outer
  | d > rI^2 = 5 -- middle
  | otherwise = 10 -- inner
 where
  rI = 1
  rM = 5
  rO = 10
  d = x^2 + y^2

