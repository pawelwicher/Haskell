module Phone (number) where
import Data.Char

cleanUp = filter isDigit

removeCountryCode [] = []
removeCountryCode ['1'] = []
removeCountryCode p@(x: xs) = if x == '1' then xs else p

validateNumber xs = length xs == 10 && xs !! 0 > '1' && xs !! 3 > '1'

number xs = if validateNumber num then Just num else Nothing
    where num = (removeCountryCode . cleanUp) xs