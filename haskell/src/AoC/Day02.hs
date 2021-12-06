{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module AoC.Day02 where

import           AoC.Util
import           Control.Applicative (Alternative (many))
import           Control.Arrow       ((&&&))
import           Text.Megaparsec     (choice)

data Dir = Forward | Down | Up deriving (Eq)
data Move = By { moveDir :: Dir, moveLen :: Int }

parser :: Parser [Move]
parser = many $ By <$> term dir <*> natural
  where dir = choice [Forward <$ "forward", Down <$ "down", Up <$ "up"]

towards :: Dir -> [Move] -> Int
towards dir = sum . fmap moveLen . filter ((== dir) . moveDir)

solveA :: [Move] -> Int
solveA = uncurry (*) . (vertical &&& horizontal)
  where
    vertical = (-) <$> towards Down <*> towards Up
    horizontal = towards Forward

-- >>> solve "2" parser solveA
-- 2091984

solveB :: [Move] -> Int
solveB xs = let (p, d, _) = foldl update (0, 0, 0) xs in p * d
  where
    update (p, d, a) = \case
      Down `By` n    -> (p, d, a + n)
      Up `By` n      -> (p, d, a - n)
      Forward `By` n -> (p + n, d + a * n, a)

-- >>> solve "2" parser solveB
-- 2086261056
