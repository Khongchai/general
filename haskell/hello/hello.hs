minList :: [Int] -> Int
minList [] = 0
minList xs = minimum xs

main :: IO ()
main = putStrLn myText

myText = "My favorite numbers are: " ++ show (minList [1, 2, 3])
