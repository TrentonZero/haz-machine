-- benchmark/MemoryMap.hs
module MemoryMapBench (benchmarks) where

import Criterion (Benchmark, bench, nf)
import MemoryMap

maxMemorySize = 1024 * 1024 -- 1 MB

trivialWriteMemoryCell = writeMemoryCell [1..10] 9 9
biggerWriteMemoryCell = writeMemoryCell [ 0 |a<-[1..100] ] 9 9
fatWriteMemoryCell = writeMemoryCell [ 0 |a<-[1..100000] ] 99999 9
maxWriteMemoryCell = writeMemoryCell [ 0 |a<-[1..maxMemorySize] ] (maxMemorySize-1) 9

trivialWriteMemory = writeMemory [1..10] 9 [9]
biggerWriteMemory = writeMemory [ 0 |a<-[1..100] ] 9 [9]
fatWriteMemory = writeMemory [ 0 |a<-[1..100000] ] 99999 [9]
maxWriteMemory = writeMemory [ 0 |a<-[1..maxMemorySize] ] (maxMemorySize-1) [9]

trivialReadMemoryCell = readMemoryCell [1..10] 9 
biggerReadMemoryCell = readMemoryCell [ 0 |a<-[1..100] ] 9 
fatReadMemoryCell = readMemoryCell [ 0 |a<-[1..100000] ] 99999 
maxReadMemoryCell = readMemoryCell [ 0 |a<-[1..maxMemorySize] ] (maxMemorySize-1) 

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
