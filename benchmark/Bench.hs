-- benchmark/Bench.hs
module Main
  (main)
  where

import Criterion.Main (bgroup, defaultMain)
import qualified MemoryMapBench

main :: IO ()
main = defaultMain [bgroup "MemoryMapBench" MemoryMapBench.benchmarks]
