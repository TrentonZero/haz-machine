module OpCodes where

import Data.Word (Word16)
import Data.Bits
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Control.Monad.ST
import Control.Monad
import Data.STRef
import MemoryMap
import ZSCIIString
import System.Exit

data OpCode
  = QUIT
  | NEW_LINE
  | NOP
  deriving (Show, Eq)


processOpCode
  :: OpCode -> MemoryMap -> MemoryMap 
processOpCode x y = advanceProgramCounter (processOpCodeInternal x y)


processOpCodeInternal
  :: OpCode -> MemoryMap -> MemoryMap
processOpCodeInternal QUIT state = updateShouldTerminate state True
processOpCodeInternal NOP state = state
processOpCodeInternal NEW_LINE state = appendToStream1 state "\n"





