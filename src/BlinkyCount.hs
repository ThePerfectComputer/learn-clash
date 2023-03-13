module BlinkyCount(topEntity, main) where
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
    t_inputs = [PortName "clk_25mhz"],
    t_output = PortName "led" }) #-}

topEntity :: Clock System -> Signal System (BitVector 8)
topEntity clock =
  exposeClockResetEnable
    (slowCounter $ SNat @5_000_000)
    clock
    resetGen
    enableGen


main :: IO ()
main = do
  putStrLn "Simulating Blinky Counter"
  print $ List.zip [0..] sim_results