{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Day16 (main) where

import Control.Parallel.Strategies (parMap, rseq)
import Data.Array (Array, bounds, listArray, (!))
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Hashable (Hashable)
import Data.Ix (inRange)
import Data.List.Extra (nubOrd)
import GHC.Generics (Generic)

data Direction = N | E | S | W deriving (Show, Ord, Eq, Generic, Hashable)

type Laser = ((Int, Int), Direction)

step :: Array (Int, Int) Char -> [Laser] -> [Laser]
step i =
  nubOrd
    . concatMap
      ( filter
          (inRange (bounds i) . fst)
          . (\l@((x, y), _) -> map (uncurry step'') $ step' l (i ! (y, x)))
      )
 where
  step' :: Laser -> Char -> [(Direction, (Int, Int))]
  step' ((x, y), d) '.' = [(d, (x, y))]
  step' ((x, y), d) '/' = case d of
    N -> [(E, (x, y))]
    E -> [(N, (x, y))]
    S -> [(W, (x, y))]
    W -> [(S, (x, y))]
  step' ((x, y), d) '\\' = case d of
    N -> [(W, (x, y))]
    E -> [(S, (x, y))]
    S -> [(E, (x, y))]
    W -> [(N, (x, y))]
  step' ((x, y), d) '|' = case d of
    N -> [(N, (x, y))]
    E -> [(N, (x, y)), (S, (x, y))]
    S -> [(S, (x, y))]
    W -> [(N, (x, y)), (S, (x, y))]
  step' ((x, y), d) '-' = case d of
    N -> [(E, (x, y)), (W, (x, y))]
    E -> [(E, (x, y))]
    S -> [(E, (x, y)), (W, (x, y))]
    W -> [(W, (x, y))]

  step'' :: Direction -> (Int, Int) -> Laser
  step'' N (x, y) = ((x, y - 1), N)
  step'' E (x, y) = ((x + 1, y), E)
  step'' S (x, y) = ((x, y + 1), S)
  step'' W (x, y) = ((x - 1, y), W)

solve :: Array (Int, Int) Char -> Laser -> Int
solve i = f mempty . iterate (step i) . pure
 where
  f :: HashSet Laser -> [[Laser]] -> Int
  f s (l : ls)
    | all (`Set.member` s) l = Set.size $ Set.map fst s
    | otherwise = f (s <> Set.fromList l) ls

main :: [String] -> IO (Int, Int)
main i = do
  let arr = listArray ((0, 0), (length (head i) - 1, length i - 1)) $ concat i
  let a = solve arr ((0, 0), E)
  let b =
        maximum
          . parMap rseq (solve arr)
          . concat
          $ [ [((x, 0), S) | x <- [0 .. length (head i) - 1]]
            , [((x, length i - 1), N) | x <- [0 .. length (head i) - 1]]
            , [((0, y), E) | y <- [0 .. length i - 1]]
            , [((length (head i) - 1, y), W) | y <- [0 .. length i - 1]]
            ]

  pure (a, b)
