{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Day10 where

import Algorithm.Search (dfs)
import Data.Bifunctor (bimap)
import Data.List (find)
import Data.List.Extra (chunksOf)
import Data.Maybe (fromJust, mapMaybe)
import Math.Geometry.Grid (Grid (indices), neighbours)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)

data Pipe = Start | NS | EW | NE | NW | SW | SE | None deriving (Eq)

instance Show Pipe where
  show :: Pipe -> String
  show = \case
    NS -> "│"
    EW -> "─"
    NE -> "└"
    NW -> "┘"
    SW -> "┐"
    SE -> "┌"
    Start -> "S"
    None -> "#"

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
            (dfs (\(a, b) -> pipeNeighbors grid pipes (a, b)) (== start))
            (pipeNeighbors grid pipes start)

  mapM_ (putStrLn . concat)
    . chunksOf (length x)
    . map
      ( \(a, b) ->
          ( if (a, b) `elem` path
              then (\x -> "\ESC[32m" <> x <> "\ESC[39;49m")
              else id
          )
            (show $ pipes !! b !! a)
      )
    $ [(a, b) | b <- [0 .. length x - 1], a <- [0 .. length (head x) - 1]]

  print start

  let a = length path - (length path `div` 2)
  pure (a, 0)