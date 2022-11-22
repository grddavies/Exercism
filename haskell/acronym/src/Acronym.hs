module Acronym (abbreviate) where

import Data.Char (toUpper, isUpper, isAlpha)

abbreviate :: String -> String
abbreviate xs = go xs ' ' "" 
  where
    go [] _ acrnm = acrnm
    go (x : xs) l acrnm = go xs x (
        if keyletter x l then acrnm ++ [toUpper x] else acrnm
        )
    keyletter x l = 
        isAlpha x && (l `elem` " -" || isUpper x && not (isUpper l))
