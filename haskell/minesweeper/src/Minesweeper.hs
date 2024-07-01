module Minesweeper (annotate) where

import Control.Applicative (Applicative (liftA2))
import Data.Char (intToDigit)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Cell a = Mine | Blank a
  deriving (Show, Eq)

instance Functor Cell where
  fmap _ Mine = Mine
  fmap f (Blank n) = Blank (f n)

instance Applicative Cell where
  pure = Blank
  Mine <*> _ = Mine
  _ <*> Mine = Mine
  Blank f <*> Blank n = Blank (f n)

type CoordMap = M.Map (Int, Int) (Cell Int)

annotate :: [String] -> [String]
annotate board = coordMapToString (buildMap board) board

addCell :: ((Int, Int), Char) -> CoordMap -> CoordMap
addCell ((i, j), cell) m = if cell == '*' then updateMap (i, j) m else m

updateMap :: (Int, Int) -> CoordMap -> CoordMap
updateMap (i, j) m = M.insert (i, j) Mine $ foldr (\(x, y) -> M.insertWith (liftA2 (+)) (x, y) (Blank 1)) m (adjacentCells (i, j))

adjacentCells :: (Int, Int) -> [(Int, Int)]
adjacentCells (i, j) = [(x, y) | x <- [(i - 1) .. (i + 1)], y <- [(j - 1) .. (j + 1)], (i, j) /= (x, y)]

buildMap :: [String] -> CoordMap
buildMap board = foldr addCell M.empty $ concatMap addRowIndices (zip [0 ..] board)
  where
    addRowIndices (i, row) = zipWith (curry (addColIndex i)) [0 ..] row
    addColIndex i (j, v) = ((i, j), v)

coordMapToString :: CoordMap -> [String] -> [String]
coordMapToString coordMap board = map buildRow [0 .. numRows - 1]
  where
    numRows = length board
    numCols = if numRows > 0 then length (head board) else 0

    buildRow :: Int -> String
    buildRow i = map (buildCell i) [0 .. numCols - 1]

    buildCell :: Int -> Int -> Char
    buildCell i j = toChar . fromMaybe (Blank 0) $ M.lookup (i, j) coordMap

    toChar :: Cell Int -> Char
    toChar Mine = '*'
    toChar (Blank 0) = ' '
    toChar (Blank n) = intToDigit n
