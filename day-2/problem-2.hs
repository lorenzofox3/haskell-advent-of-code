import Day2Lib (gamePoint, plays, parseInput, mapRule)

main = do
    contents <- getContents
    print $ sum . map gamePoint . plays . map mapRule . parseInput $ contents