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
  deriving (Show, Eq)


processOpCode 
  :: OpCode -> MemoryMap -> MemoryMap
processOpCode QUIT state = updateShouldTerminate state True 
