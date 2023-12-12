module Day09 (main) where

import Data.List.Extra (sumOn')
import Data.Tuple.Extra (both, swap)

main :: [String] -> IO (Int, Int)
main =
  pure
    . swap
    . both
      ( sumOn'
          ( sumOn'
              last
              . takeWhile (not . null)
              . iterate (uncurry (zipWith (-)) . (>>=) tail (,))
          )
      )
    . (>>=) (map reverse) (,)
    . map (map read . words)