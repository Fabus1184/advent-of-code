{-# LANGUAGE TemplateHaskell #-}

module Y2023 (solutions) where

import TH (mkSolutions)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11

solutions :: [(Int, [String] -> IO (String, String))]
solutions = $(mkSolutions)
