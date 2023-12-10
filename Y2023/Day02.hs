{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Day02 where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Lens (set)
import Control.Lens.Tuple (_1, _2, _3)
import Data.Char (isDigit, toUpper)
import Data.Function.Syntax ((<<~>))
import Data.List (findIndices)
import Data.List.Extra (wordsBy)
import Data.Tuple.Extra (both, uncurry3)
import GHC.Generics (Generic, Generic1)
import Generic.Data (Generically (..), Generically1 (..))

data Set a = Set a a a
  deriving (Show, Functor, Generic, Generic1, Foldable, Eq)
  deriving (Applicative) via (Generically1 Set)
  deriving (Semigroup, Monoid) via (Generically (Set a))

data Color = R | G | B deriving (Read)

readGame :: String -> Set Int
readGame =
  foldl1
    (liftA2 max)
    . map (uncurry3 Set)
    . uncurry (zipWith ((<<~>) set (0, 0, 0) . (\case R -> _1; G -> _2; B -> _3)))
    . (&&&)
      (map (read . pure . toUpper . head) . wordsBy (`notElem` ['r', 'g', 'b']))
      (map read . tail . wordsBy (not . isDigit))

main :: [String] -> IO (Int, Int)
main =
  pure
    . both sum
    . (&&&)
      (map succ . findIndices (and . (<<~>) liftA2 (Set 12 13 14) (<=)))
      (map product)
    . map readGame
