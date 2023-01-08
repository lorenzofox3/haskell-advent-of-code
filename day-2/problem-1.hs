import Day2Lib (gamePoint , plays, parseInput)

main = do
    contents <- getContents
    print $ sum . map gamePoint . plays . parseInput $ contents