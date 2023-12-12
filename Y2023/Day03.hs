{-# LANGUAGE TupleSections #-}

module Day03 (main) where

import Control.Arrow ((&&&))
import Control.Lens (imap)
import Data.Char (isDigit)
import Data.Function.Apply ((-$))
import Data.List.Extra (elemIndex, findIndices, groupSort)
import Data.Tuple.Extra (both, fst3, snd3, thd3)
import Math.Geometry.Grid (neighbours)
import Math.Geometry.Grid.Octagonal (rectOctGrid)

groupConsecutive :: [Int] -> [[Int]]
groupConsecutive = foldr f []
 where
  f x [] = [[x]]
  f x (y : ys)
    | x + 1 == head y = (x : y) : ys
    | otherwise = [x] : y : ys

main :: [String] -> IO (Int, Int)
main =
  pure
    . both sum
    . (&&&)
      (map fst3 . filter (not . all (uncurry (||) . (&&&) isDigit (== '.')) . snd3))
      ( map (product . snd)
          . filter ((== 2) . length . snd)
          . groupSort
          . map (thd3 &&& fst3)
      )
    . map
      ( \((ns, gr), (c, rs)) ->
          (\(a, b) -> (read (map (\r -> gr !! c !! r) rs), a, (b !!) <$> elemIndex '*' a))
            . unzip
            . map ((>>=) (uncurry $ (!!) . (gr !!)) (,))
            . concatMap (ns . (c,))
            $ rs
      )
    . uncurry (map . (,))
    . (>>=) (concat . imap (map . (,)) . map (groupConsecutive . findIndices isDigit) . snd) ((,) -$)
    . (>>=) (neighbours . uncurry rectOctGrid . (length &&& length . head)) (,)
