module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

data Clock = Clock {
    hours :: Int,
    mins :: Int
  } deriving (Eq, Show)

fromHourMin :: Int -> Int -> Clock
fromHourMin hh mm = Clock { hours=hh', mins=mm' }
  where
    hh' = (hh + q) `mod` 24
    (q, mm') = mm `divMod` 60

toString :: Clock -> String
toString (Clock hh mm) = printf "%02d:%02d" hh mm

addDelta :: Int -> Int -> Clock -> Clock
addDelta hh mm (Clock hh' mm') = fromHourMin (hh' + hh) (mm' + mm)
