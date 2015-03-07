-- benchmark/MemoryMap.hs
module MemoryMapBench (benchmarks) where

import Criterion (Benchmark, bench, nf)
import MemoryMap
import Data.Vector.Unboxed

maxMemorySize = 1024 * 1024 -- 1 MB
smallVector = fromList [1..10]
biggerVector =  fromList [0 | a<-[1..100] ]
fatVector = fromList [ 0 | a<-[1..100000] ]
maxVector = fromList [ 0 | a<-[1..maxMemorySize] ]

trivialWriteMemoryCell = writeMemoryCell smallVector 9 9
biggerWriteMemoryCell = writeMemoryCell biggerVector 9 9
fatWriteMemoryCell = writeMemoryCell fatVector 99999 9
maxWriteMemoryCell = writeMemoryCell maxVector (maxMemorySize-1) 9

trivialWriteMemory = writeMemory smallVector 9 [9]
biggerWriteMemory = writeMemory biggerVector 9 [9]
fatWriteMemory = writeMemory fatVector 99999 [9]
maxWriteMemory = writeMemory maxVector (maxMemorySize-1) [9]

trivialReadMemoryCell = readMemoryCell smallVector 9 
biggerReadMemoryCell = readMemoryCell biggerVector 9 
fatReadMemoryCell = readMemoryCell fatVector 99999 
maxReadMemoryCell = readMemoryCell maxVector (maxMemorySize-1) 

benchmarks :: [Benchmark]
benchmarks =
    [ 
    bench "trivial writeMemoryCell" (nf (const trivialWriteMemoryCell) ()),
    bench "bigger writeMemoryCell" (nf (const biggerWriteMemoryCell) ()),
    bench "fat writeMemoryCell" (nf (const fatWriteMemoryCell) ()),
    bench "max writeMemoryCell" (nf (const maxWriteMemoryCell) ()),

    bench "trivial writeMemory" (nf (const trivialWriteMemory) ()),
    bench "bigger writeMemory" (nf (const biggerWriteMemory) ()),
    bench "fat writeMemory" (nf (const fatWriteMemory) ()),
    bench "max writeMemory" (nf (const maxWriteMemory) ()),

    bench "trivial readMemoryCell" (nf (const trivialReadMemoryCell) ()),
    bench "bigger readMemoryCell" (nf (const biggerReadMemoryCell) ()),
    bench "fat readMemoryCell" (nf (const fatReadMemoryCell) ()),
    bench "max readMemoryCell" (nf (const maxReadMemoryCell) ())
    ]
