module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = rem_year 400 == 0 || (rem_year 4 == 0 && rem_year 100 /= 0)
  where rem_year = rem year
