-- | You can program the FPGA with the following.
-- | 
-- | $cd ulx3s/
-- | $TOP_ENTITY_MODULE=Blinky make
-- | The FPGA should now be blinking.
-- | 
-- | You can run the main simulation with the following.
-- | $stack run clash -- src/Blinky.hs -main-is Blinky.main -o out/main
-- | $./out/main
module Blinky(topEntity, main) where
import qualified Data.List as List
import Clash.Prelude

blinky :: forall n dom. HiddenClockResetEnable dom
  => 1 <= n
  => SNat n
  -> Signal dom (BitVector 8)
blinky toggle_period = 
    (prepend7Zeros . bitToBool) <$> 
    oscillate False toggle_period
    where
        prepend7Zeros = ((0 :: BitVector 7) ++#)
        bitToBool :: Bool -> BitVector 1
        bitToBool bit = pack bit

sim_results :: [BitVector 8]
sim_results = sampleN @System 64 $ blinky $ SNat @2


-- https://github.com/clash-lang/clash-compiler/blob/master/examples/Blinker.hs#L20
{-# ANN topEntity
   (Synthesize{
    t_name = "top",
    t_inputs = [PortName "clk_25mhz"],
    t_output = PortName "led" }) #-}
topEntity :: Clock System -> Signal System (BitVector 8)
topEntity clock =
  exposeClockResetEnable
    (blinky $ SNat @5_000_000)
    clock
    resetGen
    enableGen


main :: IO ()
main = do
  putStrLn "Simulating Blinky"
  print $ List.zip [0..] sim_results