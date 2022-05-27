main :: IO ()
main = do
  putStrLn "You are in a fridge. What do you want to do?"
  putStrLn "1. Try to get out."
  putStrLn "2. Eat."
  putStrLn "3. Die."
  command <- getLine
  case command of
    "1" ->
        putStrLn "You try to get out. You fail. You die."
    "2" ->
        do
          putStrLn "You eat. You eat some more."
          putStrLn "Damn, this food is tasty!"
          putStrLn "You eat so much you die."
    "3" ->
        putStrLn "You die."
    _   ->
        putStrLn "Did not understand."
  putStrLn "Play again? write y if you do."
  playAgain <- getLine
  if playAgain == "y"
  then main
  else putStrLn "Thanks for playing."