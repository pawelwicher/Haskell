module Lib where
    import Data.List
    import Data.List.Split

    data Person = Person Int String String | UnknownPerson String


    helloWorld :: String
    helloWorld = "Hello World!"

    hello2 :: Int
    hello2 = 2

    hello3 :: Int
    hello3 = 3

    simpleFunc1 :: Integer -> Integer
    simpleFunc1 x = p + q + x
        where p = 1
              q = 2

    getId :: Person -> Int
    getId (Person pid _ _) = pid
    getId (UnknownPerson _) = 0

    makeList :: Integer -> [Integer] -> [Integer]
    makeList 0 xs = xs
    makeList n xs = n : makeList (n-1) xs

    getFileContent :: FilePath -> IO String
    getFileContent path = readFile path

    -- cabal install split
    getLines :: [Char] -> [[Char]]
    getLines content = splitOn "\n" content

    author1 :: ([Char], [Char])
    author1 = ("Will", "Kurt")

    author2 :: ([Char], [Char])
    author2 = ("Ian", "Curtis")

    compareLastNames :: Ord a1 => (a2, a1) -> (a3, a1) -> Ordering
    compareLastNames name1 name2 = if lastName1 > lastName2 then
                                     GT
                                   else
                                     if lastName1 < lastName2 then
                                       LT
                                     else
                                       EQ
      where lastName1 = snd name1
            lastName2 = snd name2

    authors :: [([Char], [Char])]
    authors = [author1, author2]

    authorsSorted :: [([Char], [Char])]
    authorsSorted = sortBy compareLastNames authors

    funWhere :: Integer -> Integer
    funWhere x = x + a + b
      where a = 1
            b = a + 1

    maybeAdd :: Num b => Maybe b -> Maybe b
    maybeAdd x = do
      val <- x
      Just (val + 1)