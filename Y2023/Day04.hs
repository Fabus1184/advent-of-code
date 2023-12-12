{-# LANGUAGE TypeApplications #-}

module Day04 (main) where

import Control.Arrow ((&&&))
import Data.List.Extra (intersect, sumOn')
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

main :: [String] -> IO (Int, Int)
main =
  pure
    . (&&&)
      (sumOn' ((2 ^) . pred) . filter (> 0))
      ( sumOn' head
          . scanl
            (\(x : xs) m -> zipWith (+) xs (replicate m x ++ repeat 0))
            (repeat 1)
      )
    . map
      ( length
          . uncurry intersect
          . splitAt 10
          . mapMaybe (readMaybe @Int)
          . words
      )
