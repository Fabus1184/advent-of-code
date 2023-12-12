module Day05 (main) where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Bifunctor (bimap)
import Data.Function.Between (inbetween)
import Data.Ix (inRange)
import Data.List.Extra (find, split, uncons)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (both)
import Debug.Trace (trace)

location :: Int -> [[((Int, Int), Int)]] -> Int
location = foldl (\i -> maybe i (\(_, o) -> i + o) . find (\(r, _) -> inRange r i))

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = undefined
pairs (x : y : xs) = (x, y) : pairs xs

main :: [String] -> IO (Int, Int)
main =
  pure
    . ( `both`
          ( \ms -> map (`location` ms)
          , \ms ss ->
              parMap
                rpar
                ( \i -> do
                    let (s, n) = pairs ss !! i
                    (\f -> trace (show n <> ": " <> show f) f)
                      . minimum
                      . map (`location` ms)
                      $ [s .. s + n - 1]
                )
                [0 .. length ss `div` 2 - 1]
          )
      )
    . (\(seeds, maps) -> minimum . inbetween maps seeds)
    . bimap
      (map read . drop 1 . words . head)
      (map (map ((\[x, y, z] -> ((y, y + z - 1), x - y)) . map read . words) . drop 1))
    . fromJust
    . uncons
    . split null