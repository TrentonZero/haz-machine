module MemoryMap where

import           Data.Binary
import           Data.Binary.Get         as DBG (getWord16be, runGet)
import           Data.Bits
import           Data.ByteString.Builder
import           Data.ByteString.Lazy    as LBS (append, empty, snoc, toStrict,
                                                 unpack)
import           Data.Maybe
import           Control.Monad
import qualified Data.Vector.Unboxed     as V
import           Data.Word               (Word16)
import           Debug.Trace

-- Requirments 1.x and 2.x: The memory map consists of a list of 2-byte Words. The VM will decide
-- how to interpret each word.
type MemoryCell = Word16
-- Sometimes the VM needs bytes
type MemoryCellByte = Word8

type Memory = V.Vector MemoryCellByte

type Location = Int
type ByteAddress = Int

--
-- ZChar interpretation will depend upon the current shift register.
data ShiftRegister
    = UPPER
    | UPPER_THEN_LOWER
    | UPPER_THEN_SYMBOL
    | LOWER
    | LOWER_THEN_UPPER
    | LOWER_THEN_SYMBOL
    | SYMBOL
    | SYMBOL_THEN_LOWER
    | SYMBOL_THEN_UPPER
    deriving (Show,Eq)

{-| The entire game state.
This includes:
  memory : All dynamic memory.
  stack : the contents of the entire stack (this includes all stack frames)
  programCounter : The current program counter (location in dynamic memory
where execution is currently present.
  stackFrames : A list of previous stacks. When a new routine is entered,
a new stack is created and the current stack should be pushed onto the
head of the "stackFrames". When a routine exits, the current stack should
be discarded and the current head of stackFrames should be popped and
return to its state.
  shiftRegister : Register that tracks the current alphabet to use.
  shouldTerminate : Should the game terminate on the next game loop
(because the quit opcode is called for one example).
-}
data MemoryMap = MemoryMap
    { memory          :: Memory
    , stack           :: [MemoryCell]
    , programCounter  :: Int
    , stackFrames     :: [[MemoryCell]]
    , shiftRegister   :: ShiftRegister
    , shouldTerminate :: Bool
    , vars            :: [Int]
    , stream1         :: String
    } deriving (Show,Eq)

defaultMemoryMap =
    MemoryMap
    { memory = V.fromList []
    , stack = []
    , programCounter = 0
    , stackFrames = []
    , shiftRegister = LOWER
    , shouldTerminate = False
    , vars = replicate 16 0
    , stream1 = []
    }

updateShouldTerminate :: MemoryMap -> Bool -> MemoryMap
updateShouldTerminate current flag =
    MemoryMap
        (memory current)
        (stack current)
        (programCounter current)
        (stackFrames current)
        (shiftRegister current)
        flag
        (vars current)
        (stream1 current)

appendToStream1 :: MemoryMap -> String -> MemoryMap
appendToStream1 current string =
    MemoryMap
        (memory current)
        (stack current)
        (programCounter current)
        (stackFrames current)
        (shiftRegister current)
        (shouldTerminate current)
        (vars current)
        (stream1 current ++ string)

advanceProgramCounter :: MemoryMap -> MemoryMap
advanceProgramCounter current =
    updateProgramCounter current (programCounter current + 1)

updateStack :: MemoryMap -> [MemoryCell] -> MemoryMap
updateStack current newStack =
    MemoryMap
        (memory current)
        newStack
        (programCounter current)
        (stackFrames current)
        (shiftRegister current)
        (shouldTerminate current)
        (vars current)
        (stream1 current)

updateProgramCounter :: MemoryMap -> Int -> MemoryMap
updateProgramCounter current newCounter =
    MemoryMap
        (memory current)
        (stack current)
        newCounter
        (stackFrames current)
        (shiftRegister current)
        (shouldTerminate current)
        (vars current)
        (stream1 current)

updateMemoryMap :: MemoryMap -> Memory -> MemoryMap
updateMemoryMap current newMemory =
    MemoryMap
        newMemory
        (stack current)
        (programCounter current)
        (stackFrames current)
        (shiftRegister current)
        (shouldTerminate current)
        (vars current)
        (stream1 current)

updateShiftRegister :: MemoryMap -> ShiftRegister -> MemoryMap
updateShiftRegister current newShiftRegister =
    MemoryMap
        (memory current)
        (stack current)
        (programCounter current)
        (stackFrames current)
        newShiftRegister
        (shouldTerminate current)
        (vars current)
        (stream1 current)

getVar :: MemoryMap -> Int -> Int
getVar current var = vars current !! var

setVar :: MemoryMap -> Int -> Int -> MemoryMap
setVar current location var =
    let triple = splitAt3 location (location + 1) (vars current)
    in MemoryMap
           (memory current)
           (stack current)
           (programCounter current)
           (stackFrames current)
           (shiftRegister current)
           (shouldTerminate current)
           ((fst3 triple ++ [var]) ++ thrd3 triple)
           (stream1 current)

-- Write a single memory cell at a given location.
writeMemoryCell
    :: MemoryMap -> Location -> MemoryCell -> MemoryMap
writeMemoryCell current loc newCell =
    let
      bytes = unpackMemoryCell (Just newCell)
      byte1 = bytes !! 0
      byte2 = bytes !! 1
      result = ((memory current) V.// [(loc, byte1)]) V.// [(loc+1, byte2)]
    in updateMemoryMap current result


-- Read just a byte
readMemoryCellByte
    :: MemoryMap -> ByteAddress -> Maybe MemoryCellByte
readMemoryCellByte current loc = memory current V.!? loc

readMemoryCell
    :: MemoryMap -> ByteAddress -> Maybe MemoryCell
readMemoryCell current loc =
  let
    byte1 = memory current V.!? loc
    byte2 = memory current V.!? (loc+1)
  in liftM2 packWord16 byte1 byte2


-- Read a single memory cell from a given location.
-- readMemoryCell
    -- :: MemoryMap -> Location -> Maybe MemoryCell
-- readMemoryCell current loc =
  -- let
    -- byte1 = memory current V.!? (loc*2)
    -- byte2 = memory current V.!? ((loc*2)+1)
  -- in if isNothing byte1 || isNothing byte2 then
    -- Nothing
  -- else
    -- Just (packWord16 (fromJust byte1) (fromJust byte2))

-- Read the requested number of memory cells
readMemoryCells
    :: MemoryMap -> Int -> Location -> [Maybe MemoryCell]
readMemoryCells current count loc = map (readMemoryCell current) [loc..loc+count-1]

readMemoryCellBytes
    :: MemoryMap -> Int -> Location -> [MemoryCellByte]
readMemoryCellBytes current count loc = concatMap unpackMemoryCell (readMemoryCells current count loc)

-- This basically splits the memory into three, and replaces the middle with the memory we intend to
-- write. Probably not the most efficient way to do it, especially given the cost of computing the
-- length of the inbound memory cell list.
writeMemory
    :: MemoryMap -> Location -> [MemoryCell] -> MemoryMap
writeMemory current loc cells =
    let
        bytes  = unpackMemoryCells cells
        zipped = zip [loc .. (loc + (length bytes))] bytes
        result = memory current V.// zipped
    in updateMemoryMap current result

pushToStack :: MemoryMap -> MemoryCell -> MemoryMap
pushToStack state cell =
    let result = cell : stack state
    in updateStack state result

updateStackHead :: MemoryMap -> (MemoryCell -> MemoryCell) -> MemoryMap
updateStackHead state function =
    let tupleStack = popFromStack state
        firstTuple = fst tupleStack
    in if isNothing firstTuple
           then state
           else let result =
                        function (fromJust firstTuple) :
                        stack (snd tupleStack)
                in updateStack state result

popFromStack :: MemoryMap -> (Maybe MemoryCell, MemoryMap)
popFromStack state =
    let result = popFromStackInt (stack state)
    in (fst result, updateStack state (snd result))

peekFromStack :: MemoryMap -> (Maybe MemoryCell, MemoryMap)
peekFromStack state =
    let result = peekFromStackInt (stack state)
    in (fst result, updateStack state (snd result))

popFromStackInt :: [MemoryCell] -> (Maybe MemoryCell, [MemoryCell])
popFromStackInt (x:stack) = (Just x, stack)
popFromStackInt _         = (Nothing, [])

peekFromStackInt :: [MemoryCell] -> (Maybe MemoryCell, [MemoryCell])
peekFromStackInt (x:stack) = (Just x, x : stack)
peekFromStackInt _         = (Nothing, [])

unpackMemoryCells :: [MemoryCell] -> [MemoryCellByte]
unpackMemoryCells = concatMap unpackWord16

unpackMemoryCell :: Maybe MemoryCell -> [MemoryCellByte]
unpackMemoryCell = concatMap unpackWord16


unpackWord16 :: Word16 -> [Word8]
unpackWord16 = LBS.unpack . toLazyByteString . word16BE


packWord16 :: Word8 -> Word8 -> Word16
packWord16  byte1 byte2 =  runGet DBG.getWord16be
                           $ foldr
                                 (LBS.append . toLazyByteString . word8)
                                 LBS.empty [byte1,byte2]


getHighMemRange :: MemoryMap -> (ByteAddress, ByteAddress)
getHighMemRange current =
  let start = (fromJust (readMemoryCell current 0x04))
      end   = V.length (memory current)
  in ((fromIntegral start), end)



  
-------  LOCAL FUNCTIONS TO HELP OUT -----------
fst3
    :: (a, b, c) -> a
fst3 (a,_,_) = a

snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b

thrd3 :: (a, b, c) -> c
thrd3 (_,_,c) = c

-- Same as split at, but instead splits the list into three.
splitAt3
    :: Int -> Int -> [a] -> ([a], [a], [a])
splitAt3 loc1 loc2 cells =
    let mytail = snd fullTail
        middle = fst fullTail
        myhead = fst fullHead
        fullHead = splitAt loc1 cells
        fullTail = splitAt (loc2 - loc1) (snd fullHead)
    in (myhead, middle, mytail)
