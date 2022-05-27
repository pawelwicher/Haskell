module Main where

import Lib
    
main :: IO ()
main = do
    putStrLn helloWorld
    putStrLn $ show (makeList 5 [])
    putStrLn $ show $ simpleFunc1 5
    content <- getFileContent "C:/Users/wichu/Documents/Coding/Haskell/Projects/TestApp/data.txt"
    putStrLn content
    putStrLn ""
    putStrLn $ show (getLines content)
    putStrLn ""
    putStrLn $ show authorsSorted