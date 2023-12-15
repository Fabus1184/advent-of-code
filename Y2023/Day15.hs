module Day15 where

import Control.Arrow ((&&&))
import Control.Lens (FoldableWithIndex (ifoldl), FunctorWithIndex (imap), imap, ix, (%~), (&), (.~))
import Data.Char (ord)
import Data.List.Extra (findIndex, split, sumOn')

data Instruction = Set String Int | Remove String deriving (Show)

readInstruction :: String -> Instruction
readInstruction s = case split (== '=') s of
  [k, v] -> Set k (read v)
  [k] -> Remove (init k)

hash :: [Char] -> Int
hash = foldl (\acc c -> ((acc + ord c) * 17) `mod` 256) 0

exec :: [[(String, Int)]] -> Instruction -> [[(String, Int)]]
exec bs (Remove k) = bs & ix (hash k) %~ filter ((/= k) . fst)
exec bs (Set k v) =
  bs
    & ix (hash k)
      %~ \s -> case findIndex ((== k) . fst) s of
        Just i -> s & ix i .~ (k, v)
        Nothing -> s ++ [(k, v)]

focusingPower :: [(String, Int)] -> Int
focusingPower = ifoldl (\i acc (_, v) -> acc + (succ i * v)) 0

main :: [String] -> IO (Int, Int)
main =
  pure
    . (&&&)
      (sumOn' hash)
      ( sum
          . imap (\i -> (succ i *) . focusingPower)
          . foldl exec (replicate 256 [])
          . map readInstruction
      )
    . split (== ',')
    . head
