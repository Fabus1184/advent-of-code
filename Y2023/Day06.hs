{-# LANGUAGE TypeApplications #-}

module Day06 (main) where

import Data.Bifunctor (bimap)
import Data.List.Extra (uncons)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (both, second, swap)

main :: [String] -> IO (Int, Int)
main =
  pure
    . swap
    . both
      ( product
          . uncurry
            ( zipWith
                ( \t ->
                    pred
                      . uncurry (-)
                      . swap
                      . bimap (floor @Double) ceiling
                      . both ((/ 2) . (+ t))
                      . (negate >>= (,))
                      . sqrt
                      . (t * t -)
                      . (* 4)
                )
            )
      )
    . both (both $ map fromIntegral)
    . (>>=)
      (both (pure . read . concatMap show))
      (,)
    . second head
    . fromJust
    . uncons
    . map (map (read @Int) . drop 1 . words)
