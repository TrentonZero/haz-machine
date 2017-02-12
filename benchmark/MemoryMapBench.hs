-- benchmark/MemoryMap.hs
module MemoryMapBench
  (benchmarks)
  where

import           Criterion           (Benchmark, bench, nf)
import qualified Data.Vector.Unboxed as V
import           MemoryMap

maxMemorySize = 1024 * 1024 -- 1 MB

smallVector = defaultMemoryMap { memory = (V.fromList [1 .. 10])}

biggerVector = defaultMemoryMap { memory = (V.fromList [1 .. 100])}

fatVector = defaultMemoryMap { memory = (V.fromList [1 .. 100000])}

maxVector = defaultMemoryMap { memory = V.fromList [1 .. maxMemorySize]}

trivialWriteMemoryCell = memory (writeMemoryCell smallVector 9 9)

biggerWriteMemoryCell = memory (writeMemoryCell biggerVector 9 9)

fatWriteMemoryCell = memory (writeMemoryCell fatVector 99999 9)

maxWriteMemoryCell =
  memory (writeMemoryCell maxVector
                          (maxMemorySize - 1)
                          9)

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

fatMap1000WriteCell =
  map
    memory
    (map (writeMemoryCell fatVector 9)
                 [1 .. 1000])

maxMap1000WriteCell =
  map
    memory
    (map
       (writeMemoryCell maxVector
                        (maxMemorySize - 1))
       [1 .. 1000])

trivialWriteMemory =
  memory (writeMemory smallVector
                      9
                      [9])

biggerWriteMemory =
  memory (writeMemory biggerVector
                      9
                      [9])

fatWriteMemory =
  memory (writeMemory fatVector
                      99999
                      [9])

maxWriteMemory =
  memory (writeMemory maxVector
                      (maxMemorySize - 1)
                      [9])

trivialReadMemoryCell = readMemoryCell smallVector

biggerReadMemoryCell = readMemoryCell biggerVector

fatReadMemoryCell = readMemoryCell fatVector

maxReadMemoryCell = readMemoryCell maxVector

benchmarks :: [Benchmark]
benchmarks =
  [bench "trivial writeMemoryCell" (nf (const trivialWriteMemoryCell) ())
  ,bench "bigger writeMemoryCell" (nf (const biggerWriteMemoryCell) ())
  ,bench "fat writeMemoryCell" (nf (const fatWriteMemoryCell) ())
  ,bench "max writeMemoryCell" (nf (const maxWriteMemoryCell) ())
  ,bench "trivial writeMemory" (nf (const trivialWriteMemory) ())
  ,bench "bigger writeMemory" (nf (const biggerWriteMemory) ())
  ,bench "fat writeMemory" (nf (const fatWriteMemory) ())
  ,bench "max writeMemory" (nf (const maxWriteMemory) ())
  ,bench "trivial readMemoryCell" (nf (const trivialReadMemoryCell) 9)
  ,bench "bigger readMemoryCell" (nf (const biggerReadMemoryCell) 9)
  ,bench "fat readMemoryCell" (nf (const fatReadMemoryCell) 99999)
  ,bench "max readMemoryCell"
         (nf (const maxReadMemoryCell)
             (maxMemorySize - 1))
  ,bench "trivial Map1000 WriteCell" (nf (const trivialMap1000WriteCell) ())
  ,bench "bigger Map1000 WriteCell" (nf (const biggerMap1000WriteCell) ())
  ,bench "fat Map1000 WriteCell" (nf (const fatMap1000WriteCell) ())
  ,bench "max Map1000 WriteCell" (nf (const maxMap1000WriteCell) ())]
