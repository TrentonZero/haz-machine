-- benchmark/MemoryMap.hs
module MemoryMapBench
  (benchmarks)
  where

import           Criterion           (Benchmark, bench, nf)
import qualified Data.Vector.Unboxed as V
import           MemoryMap

maxMemorySize :: Location
maxMemorySize = 1024 * 1024  -- 1 MB

maxMemorySizeInt = 1024 * 1024  -- 1 MB

smallVector = defaultMemoryMap { memory = (V.fromList [1 .. 10])}

biggerVector = defaultMemoryMap { memory = (V.fromList [1 .. 100])}

trivialWriteMemoryCell = memory (writeMemoryCell smallVector 9 9)

biggerWriteMemoryCell = memory (writeMemoryCell biggerVector 9 9)



trivialMap1000WriteCell =
  map
    memory
    (map (writeMemoryCell smallVector 9)
                 [1 .. 1000])

biggerMap1000WriteCell =
  map
    memory
    (map (writeMemoryCell biggerVector 9)
                 [1 .. 1000])


trivialWriteMemory =
  memory (writeMemory smallVector
                      9
                      [9])

biggerWriteMemory =
  memory (writeMemory biggerVector
                      9
                      [9])


trivialReadMemoryCell = readMemoryCell smallVector

biggerReadMemoryCell = readMemoryCell biggerVector


benchmarks :: [Benchmark]
benchmarks =
  [bench "trivial writeMemoryCell" (nf (const trivialWriteMemoryCell) ())
  ,bench "bigger writeMemoryCell" (nf (const biggerWriteMemoryCell) ())
  ,bench "trivial writeMemory" (nf (const trivialWriteMemory) ())
  ,bench "bigger writeMemory" (nf (const biggerWriteMemory) ())
  ,bench "trivial readMemoryCell" (nf (const trivialReadMemoryCell) 9)
  ,bench "bigger readMemoryCell" (nf (const biggerReadMemoryCell) 9)
  ,bench "trivial Map1000 WriteCell" (nf (const trivialMap1000WriteCell) ())
  ,bench "bigger Map1000 WriteCell" (nf (const biggerMap1000WriteCell) ())]
