{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module AoC.Day03 where

import           AoC.Util
import           Control.Applicative  (Alternative (many, some, (<|>)))
import           Control.Arrow        ((&&&))
import           Data.List            (transpose)
import           Data.Monoid          (Sum (getSum))
import           Data.Tuple.Extra     (both)
import           Text.Megaparsec.Char (space)

data Bit = Zero | One deriving (Eq, Ord, Enum)
type Bin = [Bit]

parser :: Parser [Bin]
parser = many (some bit <* space)
  where bit = One <$ "1" <|> Zero <$ "0"

common :: Bin -> Bit
common = greater . count
  where
    greater (x, y) = if x > y then Zero else One
    count = both getSum . foldMap \case
      Zero -> (1, 0)
      One  -> (0, 1)

uncommon :: Bin -> Bit
uncommon = neg . common
  where
    neg One  = Zero
    neg Zero = One

toInt :: Bin -> Int
toInt = foldl (\n -> (n * 2 +) . num) 0
  where
    num One  = 1
    num Zero = 0

solveA :: [Bin] -> Int
solveA = uncurry (*) . both toInt . (gamma &&& epsilon)
  where
    gamma = fmap common . transpose
    epsilon = fmap uncommon . transpose

-- >>> solve "3" parser solveA
-- 3882564

support :: (Bin -> Bit) -> [Bin] -> Bin
support decide = go 0
  where
    column xs = (transpose xs !!)
    go _ [x] = x
    go n xs  = go (n + 1) $ filter ((decide (xs `column` n) /=) . (!! n)) xs

solveB :: [Bin] -> Int
solveB = uncurry (*) . both toInt . (oxygen &&& co2)
  where
    oxygen = support common
    co2 = support uncommon

-- >>> solve "3" parser solveB
-- 3385170
