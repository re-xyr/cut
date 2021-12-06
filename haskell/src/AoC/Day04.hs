{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module AoC.Day04 where

import           AoC.Util
import           Control.Applicative        (Alternative (many))
import           Control.Monad              (join)
import           Control.Monad.State.Strict (State, evalState, gets, modify,
                                             put)
import           Data.List                  (transpose)
import           Text.Megaparsec            (count, sepBy)
import           Text.Megaparsec.Char.Lexer (decimal)

data Mark = On { marked :: Bool, number :: Int } deriving (Eq)
type Board = [[Mark]]

parser :: Parser ([Int], [Board])
parser = (,) <$> draws <*> many board
  where
    draws = term $ decimal `sepBy` ","
    board = count 5 (count 5 ((False `On`) <$> natural))

winning :: Board -> Bool
winning = (||) <$> bingo <*> (bingo . transpose)
  where bingo = any (all marked)

score :: Int -> Board -> Int
score n = (n *) . sum . fmap number . filter (not . marked) . join

mark :: Int -> Board -> Board
mark n = fmap $ fmap go
  where go (b `On` m) = if m == n then True `On` m else b `On` m

victory :: [Int] -> State [Board] Int
victory [] = error "no winning board"
victory (x : xs) = do
  modify $ fmap $ mark x
  gets (fmap (score x) . filter winning) >>= \case
    []    -> victory xs
    v : _ -> pure v

solveA :: ([Int], [Board]) -> Int
solveA (d, b) = evalState (victory d) b

-- >>> solve "4" parser solveA
-- 39902

loss :: [Int] -> State [Board] Int
loss [] = error "no losing board"
loss (x : xs) = do
  modify $ fmap $ mark x
  rst <- gets $ filter $ not . winning
  win <- gets $ filter winning
  if null rst
    then pure $ score x $ head win
    else put rst >> loss xs

solveB :: ([Int], [Board]) -> Int
solveB (d, b) = evalState (loss d) b

-- >>> solve "4" parser solveB
-- 26936
