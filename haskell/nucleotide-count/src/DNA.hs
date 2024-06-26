{-# LANGUAGE ImportQualifiedPost #-}

module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map)
import Data.Map qualified as M
import Text.Printf (printf)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideFromChar :: Char -> Either Char Nucleotide
nucleotideFromChar x = case x of
  'A' -> Right A
  'C' -> Right C
  'G' -> Right G
  'T' -> Right T
  c -> Left c

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = buildMap <$> traverse (fmtError . nucleotideFromChar) xs
 where
  fmtError :: Either Char Nucleotide -> Either String Nucleotide
  fmtError (Left c) = Left $ printf "Invalid nucleotide: %c" c
  fmtError (Right n) = Right n

  buildMap :: [Nucleotide] -> Map Nucleotide Int
  buildMap codes = M.fromList [(A, countCode A), (C, countCode C), (G, countCode G), (T, countCode T)]
   where
    countCode x = length $ filter (== x) codes
