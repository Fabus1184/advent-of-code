{-# LANGUAGE NumericUnderscores #-}

module Day11 (main) where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Foldable.Extra (sumOn')
import Data.List (transpose)
import Math.Geometry.Grid (Grid (indices))
import Math.Geometry.Grid.Square (rectSquareGrid)

expandEmpty :: Int -> [[Char]] -> [[Char]]
expandEmpty n = transpose . concatMap f . transpose . concatMap f
 where
  f :: [Char] -> [[Char]]
  f x
    | all (== '.') x = replicate n x
    | otherwise = [x]

solve :: Int -> [[Char]] -> Int
solve n u = do
  let u' = expandEmpty n u
  (`div` 2)
    . sumOn'
      (\(xs, ys) -> abs (foldl1 (-) xs) + abs (foldl1 (-) ys))
    . map unzip
    . replicateM 2
    . filter (\(a, b) -> u' !! b !! a == '#')
    . indices
    . uncurry rectSquareGrid
    . (&&&) length (length . head)
    $ u'

main :: [String] -> IO (Int, Int)
main x = do
  let f n = n * (solve 1 x - solve 0 x) + solve 0 x
  pure (f 2, f 1_000_000)
