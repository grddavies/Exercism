module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = 
    let 
        divby_400 = mod year 400 == 0
        divby_100 = mod year 100 == 0
        divby_4 = mod year 4 == 0
    in
    divby_400 || divby_4 && not divby_100
