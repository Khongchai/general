module Utils where

import Data.Maybe (listToMaybe)

isEmpty :: [a] -> Bool
isEmpty array =
  case listToMaybe array of
    Nothing -> True
    Just _ -> False
