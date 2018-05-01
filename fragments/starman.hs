check :: String -> String -> Char -> (Bool, String)
check word display c
  = (c `elem` word, [if x==c
          then c
          else y | (x,y) <- zip word display])

mkguess :: String -> String -> Int -> IO()
mkguess word display n =
  do putStrLn (display ++ "  " ++ replicate n '*')
     putStr "  Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (head q)
     let n' = if correct then n else n-1
     turn word display' n'

turn :: String -> String -> Int -> IO ()
turn word display n
    | n==0              = putStrLn "You lose"
    | word == display   = putStrLn "You win!"
    | otherwise         = mkguess word display n

starman :: String -> Int -> IO ()
starman word = turn word $ replicate (length word) '-'

getGuess :: String -> Int -> Char
getGuess display n =
  do putStrLn (display ++ "  " ++ replicate n '*')
     putStr "  Enter your guess: "
     l <- getLine
     return $ head l

doTurn :: String -> String -> Int -> IO ()
doTurn word display n
    | n==0              = putStrLn "You lose"
    | word == display   = putStrLn "You win!"
    | otherwise         = 
        let c = getGuess display n
            (correct, display') = check word display c
            n' = if correct then n else n - 1
        in doTurn word display' n'

starman1 :: String -> Int -> IO ()
starman1 word = turn word $ replicate (length word) '-'
