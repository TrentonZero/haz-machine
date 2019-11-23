module Main where

import qualified Data.ByteString     as B
import           Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified MemoryMap           as MM
import qualified OpCodes             as OC
import           System.IO
import           Control.Monad

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
  let firstOpCodeByte = head (MM.unpackWord16 (fromJust firstOpCodeBytes))
  let firstOpCodeForm = liftM OC.getOpCodeForm firstOpCodeBytes
  let firstOpCodeOperandTypes = liftM2 OC.getOperandTypes firstOpCodeForm firstOpCodeBytes

  let zMachineVersion = (fromJust (MM.readMemoryCellByte mem 0x00))
  let firstOpCode = OC.getOpCode newmem

  print ("Read into memory map. Dynamic memory is " ++ (show dynMemStart)  ++ " to " ++ (show dynMemEnd))
  print ("Read into memory map. Static memory is " ++ (show staticMemStart)  ++ " to " ++ (show staticMemEnd))
  print ("Read into memory map. High memory Range is " ++ (show highMemRange))
  print ("Read into memory map. Initial PC " ++ (show initialProgramCount))
  print ("Read into memory map. Z Machine Version " ++ (show zMachineVersion))

  print ("First opcode bytes: " ++ (show firstOpCodeBytes))
  print ("First opcode byte: " ++ (show (firstOpCodeByte)))
  print ("First opcode: " ++ (show firstOpCode))
  print ("First opcode: " ++ (show firstOpCodeForm))
  print ("First opcode operand types: " ++ (show firstOpCodeOperandTypes))

  let stateAfterFirstOpcode = OC.processOpCodeInternal firstOpCode newmem
  print ("PC after first opcode: " ++ (show (MM.programCounter stateAfterFirstOpcode)))
  
  let sndOpCodeBytes = MM.readMemoryCell stateAfterFirstOpcode (MM.programCounter stateAfterFirstOpcode)
  let sndOpCode = OC.getOpCode stateAfterFirstOpcode

  let sndOpCodeByte = head (MM.unpackWord16 (fromJust sndOpCodeBytes))
  let sndOpCodeForm = liftM OC.getOpCodeForm sndOpCodeBytes
  let sndOpCodeOperandTypes = liftM2 OC.getOperandTypes sndOpCodeForm sndOpCodeBytes
  
  print ("Snd opcode bytes: " ++ (show sndOpCodeBytes))
  print ("Snd opcode byte: " ++ (show (sndOpCodeByte)))
  print ("Snd opcode: " ++ (show sndOpCode))
  print ("Snd opcode: " ++ (show sndOpCodeForm))
  print ("Snd opcode operand types: " ++ (show sndOpCodeOperandTypes))




