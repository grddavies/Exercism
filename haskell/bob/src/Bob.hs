module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs
  | null nospace = "Fine. Be that way!"
  | shouting && question = "Calm down, I know what I'm doing!"
  | question = "Sure."
  | shouting = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
      nospace = filter (not . isSpace) xs
      alphas = filter isAlpha xs
      question = last nospace == '?'
      shouting = all isUpper alphas && not (null alphas)

