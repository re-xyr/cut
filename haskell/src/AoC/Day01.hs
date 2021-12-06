module AoC.Day01 where

import           AoC.Util
import           Control.Applicative (Alternative (many))

parser :: Parser [Int]
parser = many natural

count :: [Int] -> Int
count xs = length $ filter (uncurry (<)) $ zip xs (drop 1 xs)

solveA :: [Int] -> Int
solveA = count

-- >>> solve "1" parser solveA
-- 1387

sliding :: Int -> [a] -> [[a]]
sliding n xs
  | len < n = []
  | otherwise = take (len - n + 1) $ go xs
    where
      len = length xs
      go lst@(_ : xs') = take n lst : go xs'
      go []            = []

solveB :: [Int] -> Int
solveB = count . fmap sum . sliding 3

-- >>> solve "1" parser solveB
-- 1362
