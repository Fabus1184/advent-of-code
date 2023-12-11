{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TH (mkSolutions) where

{-# HLINT ignore "Use bimap" #-}

import Control.Monad.Extra (mapMaybeM)
import Formatting (Buildable, build, formatToString)
import Language.Haskell.TH (Exp, Q, appE, listE, lookupValueName, varE)
import Text.Printf (printf)

tostring :: (Buildable a) => a -> String
tostring = formatToString build

mkSolutions :: Q Exp
mkSolutions =
  mapMaybeM
    (\n -> ((n,) <$>) <$> lookupValueName (printf "Day%02d.main" n))
    [1 :: Int .. 25]
    >>= listE . map (\(a, b) -> appE [|(,) a . (.) (fmap (\(x, y) -> (tostring x, tostring y)))|] (varE b))
