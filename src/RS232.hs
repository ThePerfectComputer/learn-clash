module RS232(topEntity, main) where
import qualified Data.List as List
import Clash.Prelude

slowCounter :: HiddenClockResetEnable dom 
            => 1 <= n
            => SNat n
            -> Signal dom (BitVector 8)
slowCounter incr_period = pack <$> count
  where
    count = regEn (0 :: Signed 8)
                  (riseEvery incr_period)
                  (count + 1)

sim_results = sampleN @System 64 $ slowCounter $ SNat @2

-- https://github.com/clash-lang/clash-compiler/blob/master/examples/Blinker.hs#L20
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
  putStrLn "Simulating Blinky Counter"
  print $ List.zip [0..] sim_results