{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module SimpleCPU where

import Clash.Prelude
import Control.Monad.State

data Instruction
  = Add
  | Sub
  deriving (Show, Generic, NFDataX)

type Register = Unsigned 8
type PC = Unsigned 8

data CPU = CPU
  { pc :: PC,
    regA :: Register,
    regB :: Register,
    aluOut :: Register
  } deriving (Show)

initialCPU :: CPU
initialCPU = CPU {pc = 0, regA = 0, regB = 2, aluOut = 0}

alu :: Instruction -> Register -> Register -> Register
alu Add a b = a + b
alu Sub a b = a - b

fetch :: PC -> Instruction
fetch 0 = Add
fetch 1 = Sub
fetch _ = error "Invalid PC"

decode :: Instruction -> Instruction
decode = id

execute :: CPU -> Instruction -> CPU
execute cpu@CPU {..} instr = cpu {aluOut = alu instr regA regB}

step :: CPU -> CPU
step cpu@CPU {..} = execute cpu (decode (fetch pc))

simulateCPU :: Int -> CPU
simulateCPU n = execState (replicateM_ n (modify step)) initialCPU

main :: IO ()
main = do
  let finalState = simulateCPU 3
  print finalState