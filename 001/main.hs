-- https://repl.it/@auburnsummer/HightechLiveCoordinates
process numbers = head [ (a, b) | a <- numbers, b <- numbers, a + b == 2020 ]
process2 numbers = head [ (a, b, c) | a <- numbers, b <- numbers, c <- numbers, a + b + c == 2020 ]
parseLines content = map read (lines content) :: [Int]

main :: IO()
main = do
    filecontent <- readFile "input"
    (putStrLn . show . process2 . parseLines) filecontent