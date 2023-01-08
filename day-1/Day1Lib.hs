module Day1Lib (
    solve1,
    solve2
) where

import Data.List

getElvesLoads :: String -> [Int]
getElvesLoads = map (sum . map read) . splitLinesByElf . lines
    where splitLinesByElf :: [String] -> [[String]]
          splitLinesByElf [] = []
          splitLinesByElf ("":xs) = splitLinesByElf xs
          splitLinesByElf xs = firstElf:(splitLinesByElf rest)
            where (firstElf, rest) = break (=="") xs

solve1 :: String -> Int
solve1 = maximum . getElvesLoads

solve2 :: String -> Int
solve2 = sum . take 3 . reverse . sort . getElvesLoads
