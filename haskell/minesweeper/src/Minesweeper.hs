module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board = map annotateRow $ enumerate board
 where
  isMine c = c == '*'
  nRows = length board
  nCols = if nRows == 0 then 0 else length $ head board
  enumerate = zip [0 ..]
  annotateRow :: (Int, String) -> String
  annotateRow (y, row) = map annotateCell $ enumerate row
   where
    annotateCell (x, cell) = if isMine cell then cell else countMines
     where
      countMines = if count > 0 then intToDigit count else ' '
      count = countMineChars $ [board !! y' !! x' | x' <- [xMin .. xMax], y' <- [yMin .. yMax]]
      countMineChars = foldl (\a b -> a + if isMine b then 1 else 0) 0
      xMin = max (x - 1) 0
      xMax = min (x + 1) (nCols - 1)
      yMin = max (y - 1) 0
      yMax = min (y + 1) (nRows - 1)
