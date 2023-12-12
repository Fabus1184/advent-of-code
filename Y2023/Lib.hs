module Lib (memoize2) where

import qualified Data.HashMap.Lazy as Map
import Data.Hashable (Hashable)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

memoize2 :: (Hashable a, Hashable b) => (a -> b -> c) -> a -> b -> c
memoize2 f x y = unsafePerformIO $ do
  mm <- readIORef m
  case Map.lookup (x, y) mm of
    Just r -> return r
    Nothing -> do
      let r = f x y
      writeIORef m (Map.insert (x, y) r mm)
      return r
 where
  m = unsafePerformIO $ newIORef Map.empty