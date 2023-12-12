module Day12 (main) where

import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Either.Extra (fromRight')
import Data.List.Extra (intercalate, sumOn')
import Lib (memoize2)
import Text.Parsec (char, choice, many1, parse, sepBy1)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Numeric (int)

parseLine :: Parser (String, [Int])
parseLine =
  liftM2
    (,)
    (many1 (choice [char '.', char '#', char '?']) <* char ' ')
    (int `sepBy1` char ',')

solve :: (String -> [Int] -> Int) -> String -> [Int] -> Int
solve f = solve'
 where
  solve' [] [] = 1
  solve' [] _ = 0
  solve' x [] = bool 1 0 ('#' `elem` x)
  solve' ('.' : xs) ns = f xs ns
  solve' x'@('#' : _) ns = j x' ns
  solve' x'@('?' : xs) ns = f xs ns + j x' ns
  j xs@(_ : x') (n : n') =
    let l = length xs
     in if n <= l
          && notElem '.' (take n xs)
          && (n == l || xs !! n /= '#')
          then f (drop n x') n'
          else 0

main :: [String] -> IO (Int, Int)
main =
  pure
    . (&&&)
      (sumOn' (uncurry solve'))
      (sumOn' (uncurry solve') . map (bimap (intercalate "?" . replicate 5) (concat . replicate 5)))
    . map (fromRight' . parse parseLine "")
 where
  solve' = memoize2 (solve solve')
