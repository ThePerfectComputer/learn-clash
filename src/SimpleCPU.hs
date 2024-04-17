{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module SimpleCPU where

import Clash.Prelude
import Control.Monad.State

import Clash.Sized.Vector (Vec(..))
import Clash.Sized.BitVector (BitVector)

-- currently memory is only 8 words deep
memInit :: [Unsigned 32]
memInit = [
  0x1,
  2,
  3,
  4,
  5]

type PC = Unsigned 64
type GPR = Vec 32 (Unsigned 64) -- General Purpose Registers page 10
type Mem n = Vec n (Unsigned 32)

data POWER_CPU = POWER_CPU
  { pc :: PC,
    gpr :: GPR
  } deriving (Show)

data Machine = Machine 
  { cpu :: POWER_CPU,
    mem :: Mem 1024
  } deriving (Show)

-- Placeholder function that currently just increments
-- the first entry in memory by 1
machine' :: Machine -> Machine
machine' machine@(Machine { cpu = cpu, mem = mem }) =
  machine { mem = replace 0 (head mem + 1) mem }


-- OLD MODEL
type Register = Unsigned 8
type PCOLD = Unsigned 64

data Instruction
  = Add
  | Sub
  deriving (Show, Generic, NFDataX)
data CPU = CPU
  { pcOld :: PCOLD,
    regA :: Register,
    regB :: Register,
    aluOut :: Register
  } deriving (Show)

initialCPU :: CPU
initialCPU = CPU {pcOld = 0, regA = 0, regB = 2, aluOut = 0}

alu :: Instruction -> Register -> Register -> Register
alu Add a b = a + b
alu Sub a b = a - b

fetch :: PCOLD -> Instruction
fetch 0 = Add
fetch 1 = Sub
fetch _ = error "Invalid PC"

decode :: Instruction -> Instruction
decode = id

execute :: CPU -> Instruction -> CPU
execute cpu@CPU {..} instr = cpu {aluOut = alu instr regA regB}

step :: CPU -> CPU
step cpu@CPU {..} = execute cpu (decode (fetch pcOld))

simulateCPU :: Int -> CPU
simulateCPU n = execState (replicateM_ n (modify step)) initialCPU

main :: IO ()
main = do
  let finalState = simulateCPU 3
  print finalState