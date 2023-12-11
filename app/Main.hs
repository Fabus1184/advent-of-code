module Main (main) where

import Advent (AoC (AoCInput), AoCOpts (..), mkDay, runAoC_)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import System.Environment (getArgs, getEnv)

import qualified Y2023 (solutions)

main :: IO ()
main =
  do
    key <- getEnv "TOKEN"
    let opts =
          AoCOpts
            { _aYear = 2023
            , _aThrottle = 1
            , _aSessionKey = key
            , _aForce = False
            , _aCache = Just "inputs"
            }
    day <- read . head <$> getArgs :: IO Int
    input <-
      lines . unpack
        <$> runAoC_
          opts
          (AoCInput . fromMaybe (error "Invalid day") . mkDay $ fromIntegral day)
    let f = fromMaybe (error "Invalid day") $ lookup day Y2023.solutions
    f input >>= (\(a, b) -> putStrLn a >> putStrLn b)
