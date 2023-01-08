module Day2Lib (
    Sign,
    GameResult,
    parseInput,
    plays,
    gamePoint,
    mapRule
) where

data Sign = Scissor | Paper | Rock deriving (Show, Eq)
data GameResult = Win | Lose | Draw deriving (Show, Eq)

gameRound :: (Sign, Sign) -> GameResult
gameRound (opponentSign, mySign)
    | opponentSign == mySign = Draw
    | otherwise = case (opponentSign, mySign) of (Paper, Rock) -> Lose
                                                 (Paper, Scissor) -> Win
                                                 (Scissor, Paper) -> Lose
                                                 (Scissor, Rock) -> Win
                                                 (Rock, Scissor) -> Lose
                                                 (Rock, Paper) -> Win

fromString :: String -> Sign
fromString "A" = Rock
fromString "X" = Rock
fromString "B" = Paper
fromString "Y" = Paper
fromString "C" = Scissor
fromString "Z" = Scissor
fromString string = error "can't match a sign"

parseInput :: String -> [(Sign, Sign)]
parseInput = map (toPair . map fromString . words) . lines
    where toPair :: [a] -> (a, a)
          toPair xs = (head xs, last xs)

plays :: [(Sign, Sign)] -> [(Sign, GameResult)]
plays = map (\game -> (snd game, gameRound(game)))

pointFromSign :: Sign -> Int
pointFromSign Rock = 1
pointFromSign Paper = 2
pointFromSign Scissor = 3

pointFromGameResult :: GameResult -> Int
pointFromGameResult Lose = 0
pointFromGameResult Draw = 3
pointFromGameResult Win = 6

gamePoint :: (Sign, GameResult) -> Int
gamePoint (sign, result) = sum [pointFromSign sign,pointFromGameResult result]

-- problem 2
-- remap the input so we get back to first problem, by translating the second part of the pair
-- X ie Rock -> I should lose
-- Y ie Paper -> draw
-- Z ie Scissor ->I should win
mapRule :: (Sign, Sign) -> (Sign, Sign)
mapRule (opponentSign, Paper) = (opponentSign, opponentSign)
mapRule (Paper, mySign) = (Paper, mySign)
mapRule (Scissor, Rock) = (Scissor, Paper)
mapRule (Scissor, Scissor) = (Scissor, Rock)
mapRule (Rock, Scissor) = (Rock, Paper)
mapRule (Rock, Rock) = (Rock, Scissor)
