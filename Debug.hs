module Main where

import Data.List
import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Monad
import System.Random
--import Data.Numbers.Primes

lenR :: [Int] -> Int
lenR [] = 0
lenR (x:xs) = 1 + lenR xs

-- sum
sumR :: [Int] -> Int
sumR [] = 0
sumR (x:xs) = x + sumR xs

-- reverse
revR :: [Int] -> [Int]
revR [] = []
revR (x:xs) = revR xs ++ [x]

-- maximum
maxR :: [Int] -> Int
maxR [x] = x
maxR (x:xs)
    | x > t = x  
    | otherwise = t  
    where t = maxR xs

reverseWords :: String -> String
reverseWords = unwords . reverse . words

isPrimeNumber :: Int -> Bool
isPrimeNumber n = length [x | x <- [2 .. n], n `mod` x == 0] == 1

getPrimes :: Int -> [Int]
getPrimes n = take n [x | x <- [2..], isPrimeNumber x]

data Person = Person { firstName :: String, lastName :: String }

instance Show Person where  
    show (Person firstName lastName) = firstName ++ " " ++ lastName

getInitials :: Person -> String 
getInitials (Person firstName lastName) = [head firstName] ++ "." ++ " " ++ [head lastName] ++ "."

rollDice :: IO Int
rollDice = getStdRandom (randomR (1, 6))

randomList :: Int -> Int -> IO [Int]
randomList a b = getStdGen >>= return . randomRs (a, b)

sumOfPositives :: [Int] -> Int
sumOfPositives = sum . filter (>0)

takeem :: Int -> [a] -> [a]
takeem 1 (x: _) = [x]
takeem m (x:xs) = x : takeem (m - 1) xs

repeatem :: a -> [a]
repeatem x = xs where xs = x : xs

replicateem :: Int -> a -> [a]
replicateem n x =  takeem n (repeatem x)

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

getProducts :: [Integer] -> [Integer] -> [Integer]
getProducts xs ys = (*) <$> ys <*> xs

double = (*2)

safeSqrt x =
    case safeSqrt' x of
        Right a -> "Sqrt(" ++ show x ++ ") = " ++ show a
        Left b -> "Error: " ++ b
    where
        safeSqrt' x
            | x < 0     = Left "negative value"
            | otherwise = Right (sqrt x)

helloWorld = do
    print "Hello"
    print "World"
    return ("test")

letTheDiceRoll = do
    print "Let the dice roll..."
    result <- rollDice
    return (result)

f 1 = 1
f 2 = 2
f n
    | n > 2 = n * 2
    | otherwise = 0

x = "hello x"

func1 =
    let x = 1
        y = 2
    in x + y

func2 =
    x + y
    where x = 10
          y = 20
       
func3 = dummy 1
    where dummy x = x + 1

getNumSign n =
    if n > 0 then 
        '+'
    else if n < 0 then
        '-'
    else
        '0'

getAtMost3 :: [Int] -> [Int]
getAtMost3 [] = []
getAtMost3 (x:y:z:xs) = [x,y,z]
getAtMost3 (x:y:xs) = [x,y]
getAtMost3 (x:xs) = [x]

guardFun [] = "empty list"
guardFun (x:_)
      | x == 0 = "zero"
      | x > 0 && even x = "even"
      | x > 0 && odd x = "odd"
      | otherwise = "negative"

quicksort [] = []
quicksort (x:xs) =
  let list1 = quicksort [a | a <- xs, a <= x]  
      list2 = quicksort [a | a <- xs, a > x]  
  in  list1 ++ [x] ++ list2

main :: IO ()
main = do
    putStrLn "Hello World from Haskell"
    print [2 ^ x | x <- [0..10]]
    print $ reverseWords "dog lazy the over jumps fox brown quick The"
    print $ getPrimes 10
    print $ sum . map digitToInt $ show 1892376
    print $ True && False
    rndList <- (randomList 0 26)
    let rndString = [(['A' .. 'Z'] ++ [' ']) !! x | x <- rndList]    
    let s1 = take 10 $ drop 10 rndString
    let s2 = take 10 $ drop 20 rndString        
    print s1
    print s2
    print $ gcd 8 12
    print $ sumOfPositives [1,-5,2]
    print $ takeem 3 [1,1,1,1,1]
    print $ takeem 4 $ repeatem 1
    print $ replicateem 5 1
    print $ take 10 fibs
    print $ getProducts [1,2,3] [100,200]
    print $ foldl1 (+) [1,2,3,1]
    print $ sum $ map (\x -> if x == Nothing then 0 else fromJust x) [Just 1, Nothing, Just 2]
    print $ Just (+1) <*> Just 2
    print $ (+) <$> Just 1 <*> Just 4
    print $ liftA2 (+) (Just 3) (Just 4)
    print $ foldl1 (\x y -> (+) <$> x <*> y) [Just 1, Just 2, Just 7]
    print $ map (\x -> Just x) [1,2,3]
    print $ double 2
    print $ double 2.5
    let person = Person "Haskell" "Curry"    
    print person
    let person2 = Person { lastName = "bar", firstName = "foo" }
    print $ firstName person2
    print $ getInitials person
    print $ safeSqrt 40
    print $ safeSqrt (-1)
    print $ (Just (-1)) >>= (\x -> if x < 0 then Nothing else Just "Zero or more")
    print $ (Just 1234) >>= (\x -> if x < 0 then Nothing else Just "Zero or more")
    test <- helloWorld
    print test
    dice <- letTheDiceRoll
    print dice
    print $ (head . tail) [0,1,2,3,4,5]
    print $ (sum . filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0)) [0 .. 10]
    print $ f 1
    print $ f 2
    print $ f 3
    print $ f (-1)
    print $ (+1) 100
    print x
    print func1
    print func2
    print func3
    print $ getNumSign (1)
    print $ getNumSign (-1)
    print $ getNumSign (0)
    print $ getAtMost3 []
    print $ getAtMost3 [1]
    print $ getAtMost3 [1,2]
    print $ getAtMost3 [1,2,3]
    print $ getAtMost3 [1,2,3,4]
    print $ guardFun []
    print $ guardFun [0,1,2]
    print $ guardFun [1,2,3]
    print $ guardFun [2,2,3]
    print $ guardFun [-1,2,3]
    print $ quicksort [1,5,2]
    --print $ take 10 primes
    forM [1,2,3] print
    return ()