{-# LANGUAGE OverloadedStrings #-}
module AoC.Day06 where

import           AoC.Util
import           Control.Applicative.Combinators (sepBy)
import           Data.IntMap.Strict              (IntMap)
import qualified Data.IntMap.Strict              as M

parser :: Parser [Int]
parser = sepBy natural ","

type School = IntMap Integer

spawn :: School -> School
spawn m = M.insertWith (+) 6 count . M.insertWith (+) 8 count . M.mapKeys pred . M.delete 0 $ m
  where count = M.findWithDefault 0 0 m

generations :: Int -> [Int] -> Integer
generations n = sum . M.elems . foldr (.) id (replicate n spawn) . foldr (flip (M.insertWith (+)) 1) M.empty

solveA, solveB :: [Int] -> Integer
solveA = generations 80
solveB = generations 256

-- >>> solve "6" parser solveA
-- 375482

-- >>> solve "6" parser solveB
-- 1689540415957
