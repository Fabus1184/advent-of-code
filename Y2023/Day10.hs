{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Day10 where

import Algorithm.Search (dfs)
import Control.Parallel.Strategies (parMap, rseq)
import Data.Bifunctor (bimap)
import Data.Ext (ext)
import Data.Geometry (Point (..))
import Data.Geometry.Polygon (fromPoints, insidePolygon)
import Data.List (find, (\\))
import Data.Maybe (fromJust, mapMaybe)
import Data.Tuple.Extra (both)
import GHC.Utils.Misc (count)
import Math.Geometry.Grid (Grid (indices), neighbours)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)

data Pipe = Start | NS | EW | NE | NW | SW | SE | None deriving (Eq)

instance Read Pipe where
  readsPrec :: Int -> ReadS Pipe
  readsPrec _ = \case
    ('|' : xs) -> [(NS, xs)]
    ('-' : xs) -> [(EW, xs)]
    ('L' : xs) -> [(NE, xs)]
    ('J' : xs) -> [(NW, xs)]
    ('7' : xs) -> [(SW, xs)]
    ('F' : xs) -> [(SE, xs)]
    ('.' : xs) -> [(None, xs)]
    ('S' : xs) -> [(Start, xs)]
    _ -> []

data Node = Node (Int, Int) Pipe

pipeNeighbors :: RectSquareGrid -> [[Pipe]] -> (Int, Int) -> [(Int, Int)]
pipeNeighbors grid pipes (a, b) =
  filter
    ( \(x, y) ->
        (x, y) `elem` neighbours grid (a, b)
          && (a, b)
            `elem` map (bimap (+ x) (+ y)) (dir (x, y))
    )
    . map (bimap (+ a) (+ b))
    $ dir (a, b)
 where
  dir (i, j) = case pipes !! j !! i of
    NS -> [(0, -1), (0, 1)]
    EW -> [(1, 0), (-1, 0)]
    NE -> [(0, -1), (1, 0)]
    NW -> [(0, -1), (-1, 0)]
    SW -> [(0, 1), (-1, 0)]
    SE -> [(0, 1), (1, 0)]
    Start -> [(0, 1), (0, -1), (1, 0), (-1, 0)]
    None -> []

main :: [String] -> IO (Int, Int)
main x = do
  let pipes = map (map (read . pure)) x :: [[Pipe]]
  let grid = rectSquareGrid (length (head pipes)) (length pipes)
  let start = fromJust $ find (\(a, b) -> pipes !! b !! a == Start) $ indices grid
  let path =
        head $
          mapMaybe
            (\s -> (s :) <$> dfs (\(a, b) -> pipeNeighbors grid pipes (a, b)) (== start) s)
            (pipeNeighbors grid pipes start)

  let a = length path - (length path `div` 2)

  let pipes' = map (both fromIntegral) (indices grid \\ path) :: [(Double, Double)]
  let poly =
        fromPoints
          . map (ext . uncurry Point2 . both fromIntegral)
          . filter (\(a, b) -> pipes !! b !! a `notElem` [NS, EW])
          $ path

  let b = count id $ parMap rseq ((`insidePolygon` poly) . uncurry Point2) pipes'
  pure (a, b)