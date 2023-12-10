{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day07 where

import Control.Arrow (first, (&&&))
import Control.Lens (FunctorWithIndex (imap))
import Control.Monad (replicateM)
import Data.Data (Typeable, eqT)
import Data.List (partition, sortOn)
import Data.List.Extra (groupSort)
import Data.Maybe (isJust)
import Data.Tuple.Extra (both)

data Part = P1 | P2

data Card (a :: Part) = J2 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T | J1 | Q | K | A deriving (Show, Eq, Ord, Enum)

data Rank = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord)

readCard :: forall a. (Typeable a) => Char -> Card a
readCard c = case c of
  '2' -> N2
  '3' -> N3
  '4' -> N4
  '5' -> N5
  '6' -> N6
  '7' -> N7
  '8' -> N8
  '9' -> N9
  'T' -> T
  'J'
    | isJust (eqT @a @'P1) -> J1
    | isJust (eqT @a @'P2) -> J2
  'Q' -> Q
  'K' -> K
  'A' -> A
  _ -> undefined

readHand :: (Typeable a) => String -> ([Card a], Int)
readHand x =
  let (h, b) = splitAt 5 x
      cs = map readCard h
   in (cs, read b)

rank1 :: [Card a] -> Rank
rank1 cs
  | g 1 5 = FiveOfAKind
  | g 1 4 = FourOfAKind
  | g 1 3 && g 1 2 = FullHouse
  | g 1 3 = ThreeOfAKind
  | g 2 2 = TwoPair
  | g 1 2 = OnePair
  | otherwise = HighCard
 where
  g x n = (x ==) . length . filter (== n) . map (sum . snd) . groupSort . map ((,1 :: Int) . fromEnum) $ cs

rank2 :: [Card 'P2] -> Rank
rank2 cs = maximum $ map (rank1 . (w ++)) $ replicateM j [J2 .. A]
 where
  (j, w) = first length $ partition (== J2) cs

main :: [String] -> IO (Int, Int)
main =
  pure
    . both (sum . imap (\i n -> succ i * n))
    . (&&&)
      (map snd . sortOn (\(cs, b) -> (rank1 cs, cs, b)) . map (readHand @'P1))
      (map snd . sortOn (\(cs, b) -> (rank2 cs, cs, b)) . map readHand)
