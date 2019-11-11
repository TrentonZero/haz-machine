module Main where

import System.IO
import Data.Maybe
import qualified Data.ByteString as B
import qualified MemoryMap as MM
import qualified OpCodes as OC
import qualified Data.Vector.Unboxed     as V

main = do
  bytestring <- B.readFile "zork1.z1"
  print ("File Read: " ++ (show (B.length bytestring)))
  let mem = MM.defaultMemoryMap { MM.memory = (V.fromList (B.unpack bytestring)) }
  let dynMemStart = 0
  let staticMemStart = (fromJust (MM.readMemoryCell mem 0x0E))
  let dynMemEnd = (staticMemStart - 1)

  let staticMemEnd = let staticMemEndOpt = MM.readMemoryCell mem 0xFFFF
        in if isNothing staticMemEndOpt then
          B.length bytestring
        else
          fromIntegral(fromJust staticMemEndOpt)

  let highMemStart = (fromJust (MM.readMemoryCell mem 0x04))
  let highMemEnd = B.length bytestring

  let initialProgramCount = (fromIntegral (fromJust (MM.readMemoryCell mem 0x06)))

  let newmem = mem { MM.programCounter = initialProgramCount }
  let firstOpCodeBytes = MM.readMemoryCell mem initialProgramCount

  let zMachineVersion = (fromJust (MM.readMemoryCellByte mem 0x00))
  let firstOpCode = OC.getOpCode newmem

  print ("Read into memory map. Dynamic memory is " ++ (show (dynMemStart))  ++ " to " ++ (show (dynMemEnd)))
  print ("Read into memory map. Static memory is " ++ (show (staticMemStart))  ++ " to " ++ (show (staticMemEnd)))
  print ("Read into memory map. High memory is " ++ (show (highMemStart))  ++ " to " ++ (show (highMemEnd)))
  print ("Read into memory map. Initial PC " ++ (show (initialProgramCount)))
  print ("Read into memory map. Z Machine Version " ++ (show (zMachineVersion)))

  print ("First opcode bytes: " ++ (show firstOpCodeBytes))
  print ("First opcode: " ++ (show firstOpCode))




