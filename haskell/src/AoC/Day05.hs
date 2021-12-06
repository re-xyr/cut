{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module AoC.Day05 where

import           AoC.Util
import           Control.Applicative (Alternative (many))
import           Data.Array          (Array, accum, elems, listArray)
import           Data.Tuple.Extra    (both)

type Vector = (Int, Int)
type Point = Vector
data Line = (:->) { from :: Point, to :: Point }

parser :: Parser [Line]
parser = many $ (:->) <$> point <* term "->" <*> point
  where point = (,) <$> natural <* "," <*> natural

type Dimensions = Vector

dimensions :: [Line] -> Dimensions
dimensions ls = both maximum . unzip $ (from <$> ls) <> (to <$> ls)

type Plane = Array Point Int

emptyPlane :: Dimensions -> Plane
emptyPlane (x, y) = listArray ((0, 0), (x, y)) $ replicate (succ x * succ y) 0

data LineV = V { origin :: Point, direction :: Vector, distance :: Int }

classify :: Line -> LineV
classify (o@(x1, y1) :-> (x2, y2)) = V o (both signum delta) (uncurry max $ both abs delta)
  where delta = (x2 - x1, y2 - y1)

fill :: LineV -> Plane -> Plane
fill (V orig dir len) pl = accum (+) pl ((, 1) . (orig ^+) . (`both` dir) . (*) <$> [0 .. len])
  where (x1, y1) ^+ (x2, y2) = (x1 + x2, y1 + y2)

avoid :: Dimensions -> [LineV] -> Int
avoid ds = length . filter (>= 2) . elems . foldr fill (emptyPlane ds)

solveA :: [Line] -> Int
solveA ls = avoid (dimensions ls) (filter isOrthogonal $ classify <$> ls)
  where isOrthogonal = (1 ==) . abs . uncurry (+) . direction

-- >>> solve "5" parser solveA
-- 7674

solveB :: [Line] -> Int
solveB ls = avoid (dimensions ls) (classify <$> ls)

-- >>> solve "5" parser solveB
-- 20898
