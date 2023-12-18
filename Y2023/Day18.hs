{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Day18 where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Either.Extra (fromRight')
import Data.Ext (ext)
import Data.Foldable.Extra (sumOn')
import Data.Functor (($>))
import Data.Geometry (Point (..), area, listEdges, segmentLength)
import Data.Geometry.Polygon (fromPoints)
import Data.Text.Internal.Read (hexDigitToInt)
import Data.Tuple.Extra (both, swap)
import Text.Parsec (char, choice, hexDigit, parse)
import Text.ParserCombinators.Parsec.Numeric (int)

data Direction = R | L | U | D deriving (Show)

data Dig = Dig Direction Int String deriving (Show)

readDig :: String -> Dig
readDig =
  fromRight'
    . parse
      ( do
          d <- choice [char 'R' $> R, char 'L' $> L, char 'U' $> U, char 'D' $> D]
          _ <- char ' '
          n <- int
          _ <- char ' '
          s <- char '(' *> char '#' *> replicateM 6 hexDigit <* char ')'
          pure (Dig d n s)
      )
      ""

reinterpretDig :: Dig -> Dig
reinterpretDig (Dig _ _ s) = Dig d' n' ""
 where
  (n, [d]) = splitAt 5 s
  d' = case d of
    '0' -> R
    '1' -> D
    '2' -> L
    '3' -> U
  n' = foldl (\a b -> a * 16 + b) 0 $ map hexDigitToInt n

runDig :: Dig -> (Int, Int) -> (Int, Int)
runDig (Dig d n _) = \case
  (x, y) -> case d of
    R -> (x - n, y)
    L -> (x + n, y)
    U -> (x, y + n)
    D -> (x, y - n)

main :: [String] -> IO (Int, Int)
main =
  pure
    . swap
    . both
      ( round @Double
          . succ
          . uncurry (+)
          . (&&&) area ((/ 2) . sumOn' segmentLength . listEdges)
          . fromPoints
          . map (ext . uncurry Point2 . both fromIntegral)
          . scanr runDig (0, 0)
      )
    . (>>=) (map reinterpretDig) (,)
    . map readDig
