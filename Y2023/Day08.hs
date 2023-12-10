{-# LANGUAGE BlockArguments #-}

module Day08 (main) where

import Control.Arrow ((&&&))
import Control.Monad.Trans.State (evalState, get, modify)
import Data.Char (isAlpha)
import Data.Function.Apply ((--$))
import Data.List.Extra (groupOn)
import Data.Map (Map, fromList, keys, (!))
import Data.Tuple (swap)

data Direction = L | R deriving (Read, Eq)

zugriff :: Map String (String, String) -> [Direction] -> String -> [String]
zugriff graph path = evalState (traverse state (cycle path))
 where
  state L = get <* modify (fst . (graph !))
  state R = get <* modify (snd . (graph !))

main :: [String] -> IO (Int, Int)
main =
  pure
    . (&&&)
      (length . takeWhile (/= "ZZZ") . uncurry (zugriff --$ "AAA"))
      ( foldl1 lcm
          . uncurry (\(g, p) -> map (length . takeWhile ((/= 'Z') . last) . zugriff g p))
          . swap
          . (>>=) (filter ((== 'A') . last) . keys . fst) (,)
      )
    . (&&&)
      ( fromList
          . map ((\[a, b, c] -> (a, (b, c))) . filter (all isAlpha) . groupOn isAlpha)
          . drop 2
      )
      (map (read . pure) . head)
