module AoC.Util where

import           Data.Maybe                 (fromJust)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, hidden, parseMaybe)
import           Text.Megaparsec.Char       (space)
import           Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

raw :: String -> IO String
raw n = readFile ("haskell/src/AoC/inputs/" ++ n ++ ".txt")

parsed :: String -> Parser a -> IO a
parsed n parser = fromJust . parseMaybe (term parser) <$> raw n

solve :: String -> Parser a -> (a -> b) -> IO b
solve n parser solution = solution <$> parsed n parser

solveIO :: String -> Parser a -> (a -> IO b) -> IO b
solveIO n parser solution = parsed n parser >>= solution

natural :: Parser Int
natural = term decimal

term :: Parser a -> Parser a
term = (<* hidden space)
