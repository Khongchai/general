minList :: [Int] -> Int
minList [] = 0
minList xs = minimum xs

main :: IO ()
main = do
  putStrLn ("My favorite numbers are: " ++ show (minList [1, 2, 3]))
