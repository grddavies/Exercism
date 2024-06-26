module Clock (addDelta, fromHourMin, toString) where

import Prelude hiding (min)

data Clock = Clock {
    hour :: Int,
    min :: Int
  } deriving (Eq, Show)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock { hour=hours, min=mins }
  where
    hours = (hour + q) `mod` 24
    (q, mins) = min `divMod` 60

toString :: Clock -> String
toString clock = (fmt $ hour clock) ++ ":" ++ (fmt $ min clock)
    where
      fmt x = if x < 10 then "0" ++ show x else show x

addDelta :: Int -> Int -> Clock -> Clock
addDelta h m clock = fromHourMin (hour clock + h) (min clock + m)
