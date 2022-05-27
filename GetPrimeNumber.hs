module Main where

import Data.Numbers.Primes

main :: IO ()
main = do
    print "Enter number:"
    --numberStr <- getLine
    let numberStr = "1000"
    let n = read $ numberStr :: Int
    let primeNumber = head . drop (n - 1) $ primes
    print $ "Prime number for " ++ show n ++ " is " ++ show primeNumber