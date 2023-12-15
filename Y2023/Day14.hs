module Day14 (main) where

import Control.Lens (imap)
import Data.List.Extra (replace, transpose)
import Data.Tuple.Extra (both, (&&&))

firstEq :: (Eq a) => [a] -> a
firstEq = fst . head . dropWhile (uncurry (/=)) . uncurry zip . (>>=) tail (,)

tilt :: [String] -> [String]
tilt = b . b . a . a
 where
  a = firstEq . iterate (map $ replace ".O" "O.") . transpose
  b = firstEq . iterate (map $ replace "O." ".O") . transpose

findCycle :: (Eq a) => [a] -> (Int, Int)
findCycle xs = floydCycle xs xs 0 0
 where
  floydCycle s f ss l
    | null f || null (tail f) = undefined
    | head s == head (tail f) = (ss, succ l)
    | otherwise = floydCycle (tail s) (tail $ tail f) (ss + 1) (l + 2)

load :: [String] -> Int
load = sum . imap (\i -> (succ i *) . length . filter (== 'O')) . reverse

main :: [String] -> IO (Int, Int)
main =
  pure
    . both load
    . (&&&)
      ( transpose
          . firstEq
          . iterate (map $ replace ".O" "O.")
          . transpose
      )
      ( \x ->
          let (s, l) = findCycle $ iterate tilt x
           in iterate tilt x !! succ (s + ((1000000000 - succ s) `mod` succ l))
      )
