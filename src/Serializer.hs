module Serializer(topEntity, main) where
import Clash.Prelude

-- TODO
{-# ANN topEntity
    (Synthesize{
     t_name = "top",
     t_inputs = [PortName "clk_25mhz",
                 PortName "ftdi_txd"],
     t_output = PortName "ftdi_rxd" }) #-}

topEntity :: Clock System -> (BitVector 1) -> (BitVector 1)
topEntity clock serial_in =
  exposeClockResetEnable
    serial_in
    clock
    resetGen
    enableGen

main :: IO ()
main = do
  putStrLn "No simulation specified."
