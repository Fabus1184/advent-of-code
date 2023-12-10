module Day01 where

import Control.Applicative (liftA2)
import Data.Char (digitToInt, isDigit)
import Data.List.Extra (replace)
import Data.Tuple.Extra (both)

calibration :: String -> Int
calibration =
  uncurry ((+) . (* 10))
    . both digitToInt
    . liftA2 (,) head last
    . filter isDigit
    . foldl1
      (.)
      ( zipWith
          (\x y -> replace x (take 2 x <> show y <> drop 2 x))
          ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
          [1 :: Int ..]
      )

main :: [String] -> IO (Int, Int)
main = pure . both (sum . map calibration) . (>>=) (map (filter isDigit)) (,)
