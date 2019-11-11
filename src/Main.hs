module Main where

import qualified Data.ByteString     as B
import           Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified MemoryMap           as MM
import qualified OpCodes             as OC
import           System.IO

main = do
  bytestring <- B.readFile "zork1.z1"
  print ("File Read: " ++ (show (B.length bytestring)))
  let mem = MM.defaultMemoryMap { MM.memory = (V.fromList (B.unpack bytestring)) }
  let dynMemStart = 0
  let staticMemStart = (fromJust (MM.readMemoryCell mem 0x0E))
  let dynMemEnd = (staticMemStart - 1)

  -- maybe :: b -> (a -> b) -> Maybe a -> b
  -- so, a Maybe of type a
  -- so, default value of type b
  -- and a function you would like to apply that works a -> b
  -- if the Maybe is Nothing, return the default value.
  -- otherwise, apply the function to it
  -- also: maybe is the stupidest function definition ever.

  let staticMemEnd = let staticMemEndOpt = MM.readMemoryCell mem 0xFFFF
        in maybe (B.length bytestring) fromIntegral staticMemEndOpt

  let highMemRange = MM.getHighMemRange mem

  let initialProgramCount = (fromIntegral (fromJust (MM.readMemoryCell mem 0x06)))

  let newmem = mem { MM.programCounter = initialProgramCount }
  let firstOpCodeBytes = MM.readMemoryCell mem initialProgramCount

  let zMachineVersion = (fromJust (MM.readMemoryCellByte mem 0x00))
  let firstOpCode = OC.getOpCode newmem

  print ("Read into memory map. Dynamic memory is " ++ (show dynMemStart)  ++ " to " ++ (show dynMemEnd))
  print ("Read into memory map. Static memory is " ++ (show staticMemStart)  ++ " to " ++ (show staticMemEnd))
  print ("Read into memory map. High memory Range is " ++ (show highMemRange))
  print ("Read into memory map. Initial PC " ++ (show initialProgramCount))
  print ("Read into memory map. Z Machine Version " ++ (show zMachineVersion))

  print ("First opcode bytes: " ++ (show firstOpCodeBytes))
  print ("First opcode: " ++ (show firstOpCode))




