module Pangram (isPangram) where

import Data.Char

isPangram :: String -> Bool
isPangram text = all (`elem` textl) alpha where
    alpha = ['a' .. 'z']
    textl = map toLower text
