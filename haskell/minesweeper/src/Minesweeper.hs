module Minesweeper (annotate) where

import Data.Char (chr, ord)

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
      countMines = if count > 0 then digit else ' '
      count = vertical + horizontal + diagonal
      digit = chr $ zero + count
      zero = ord '0'

      vertical = foldl (\a b -> a + if isMine b then 1 else 0) 0 [above, below]
       where
        above = if y > 0 then (board !! (y - 1)) !! x else ' '
        below = if y < (nRows - 1) then (board !! (y + 1)) !! x else ' '

      horizontal = foldl (\a b -> a + if isMine b then 1 else 0) 0 [left, right]
       where
        left = if x > 0 then row !! (x - 1) else ' '
        right = if x < (nCols - 1) then row !! (x + 1) else ' '

      diagonal = foldl (\a b -> a + if isMine b then 1 else 0) 0 [topLeft, topRight, bottomLeft, bottomRight]
       where
        topLeft = if x > 0 && y > 0 then board !! (y - 1) !! (x - 1) else ' '
        topRight = if x < (nCols - 1) && y > 0 then board !! (y - 1) !! (x + 1) else ' '
        bottomLeft = if x > 0 && y < (nRows - 1) then board !! (y + 1) !! (x - 1) else ' '
        bottomRight = if x < (nCols - 1) && y < (nRows - 1) then board !! (y + 1) !! (x + 1) else ' '
