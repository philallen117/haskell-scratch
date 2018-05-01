module Mastermind
    ( mastermind
    ) where

data Colour = R | G | B | Y | W deriving (Read, Show, Eq)

charToString :: Char -> String
charToString = (:[])

type Row = [Colour]

rowSize :: Int
rowSize = 5

readRow :: String -> Row
readRow str =
    if length str /= rowSize
        then error "Ill-formed string."
        else map (read . charToString) str

data Answer = Answer {
    blacks :: Int,
    whites :: Int
} deriving (Show)

correct :: Answer -> Bool
correct a = blacks a == rowSize

check :: Row -> Row -> Answer
check problem guess = Answer { blacks = pm, whites = cm - pm}
  where
    pm = placeMatches problem guess
    cm = colourMatches problem guess

    placeMatches :: Row -> Row -> Int
    placeMatches r1 r2 = foldr ((+) . fromEnum) 0 (zipWith (==) r1 r2)

    colourMatches :: Row -> Row -> Int
    colourMatches r1 r2 = sum $ map (matchesOnCol r1 r2) [R, G, B, Y, W]

    matchesOnCol :: Row -> Row -> Colour -> Int
    matchesOnCol r1 r2 c = min (countCol r1 c) (countCol r2 c)

    countCol :: Row -> Colour -> Int
    countCol r c = length $ filter (c ==) r


turn :: Row -> Int -> IO ()
turn problem turns =
    do  putStrLn ("  Turn:  " ++ show turns)
        putStr "  Enter your guess: "
        l <- getLine
        let guess = readRow l
        let answer = check problem guess
        if correct answer
            then putStrLn "Congratulations, that's correct."
            else do print answer
                    turn problem (turns + 1)

mastermind :: String -> IO()
mastermind problemString = turn (readRow problemString) 1
