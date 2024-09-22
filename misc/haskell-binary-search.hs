binarySearch :: (Ord a) => [a] -> a -> Maybe Int
binarySearch xs target = binarySearchHelper xs target 0 (length xs - 1)

binarySearchHelper :: (Ord a) => [a] -> a -> Int -> Int -> Maybe Int
binarySearchHelper xs target low high
  | low > high = Nothing
  | otherwise =
      let mid = (low + high) `div` 2
       in case compare (xs !! mid) target of
            EQ -> Just mid
            LT -> binarySearchHelper xs target (mid + 1) high
            GT -> binarySearchHelper xs target low (mid - 1)

-- Example usage:
main :: IO ()
main = do
  let sortedList = [1, 3, 5, 7, 9, 11, 13, 15]
  print $ binarySearch sortedList 7 -- Should return Just 3
  print $ binarySearch sortedList 6 -- Should return Nothing