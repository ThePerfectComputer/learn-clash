module RS232(topEntity, main) where
import qualified Data.List as List
import Util((.<<+))
import Clash.Prelude

data SerializerState = IDLE | START | DATA (Index 8) | PARITY | STOP
  deriving (Generic, Show, Eq, NFDataX)

data FTDIClockState hi = NotRunning | Running (Index hi)
  deriving (Generic, Show, Eq, NFDataX)

isHalfCycle :: forall n . KnownNat n =>
               1 <= n =>
               FTDIClockState n -> Bool
isHalfCycle (Running cnt) = cnt == halfMaxVal
  where halfMaxVal = div (maxBound :: Index n) 2
isHalfCycle NotRunning    = False

ftdiClock :: forall n dom . HiddenClockResetEnable dom =>
             (1 <= n) =>
             Signal dom Bool ->
             SNat n ->
             Signal dom (FTDIClockState n)
ftdiClock en SNat = sig
  where sig = register NotRunning (sig' <$> en <*> sig)
        sig' :: Bool -> FTDIClockState n -> FTDIClockState n
        sig' False _            = NotRunning
        sig' True NotRunning    = Running 0
        sig' True (Running val) = Running $ satSucc SatWrap val

predicatedDeSerialize :: SerializerState ->
                         BitVector 8 ->
                         Bit ->
                         BitVector 8
predicatedDeSerialize START _ _                = 0
predicatedDeSerialize IDLE  _ _                = 0
predicatedDeSerialize (DATA _)bitVec8In bitIn  = bitVec8In .<<+ bitIn
predicatedDeSerialize STOP   bitVec8In _       = bitVec8In
predicatedDeSerialize PARITY bitVec8In _       = bitVec8In

slowCounter :: HiddenClockResetEnable dom 
            => 1 <= n
            => SNat n
            -> Signal dom (BitVector 8)
slowCounter incr_period = pack <$> count
  where
    count = regEn (0 :: Signed 8)
                  (riseEvery incr_period)
                  (count + 1)

clockDivider :: forall n dom. HiddenClockResetEnable dom
  => 1 <= n
  => SNat n
  -> Signal dom (BitVector 1)
clockDivider SNat =
  let cntr = register (0 :: Index n) $ satSucc SatWrap <$> cntr
  in register 0 $ pack <$> (cntr .==. 0)

sim_results = sampleN @System 64 $ slowCounter $ SNat @2

serial_loopback :: HiddenClockResetEnable dom => 
                   Signal dom (BitVector 1) ->
                   Signal dom (BitVector 1)
serial_loopback serial_in = register 0 serial_in

-- https://github.com/clash-lang/clash-compiler/blob/master/examples/Blinker.hs#L20
{-# ANN topEntity
   (Synthesize{
    t_name = "top",
    t_inputs = [PortName "clk_25mhz",
                PortName "ftdi_txd"],
    t_output =  PortName "ftdi_rxd" }) #-}
{-# NOINLINE topEntity #-}
topEntity :: Clock System -> 
             Signal System (BitVector 1) ->
             Signal System (BitVector 1)
topEntity clock serial_in =
  exposeClockResetEnable
    (serial_loopback serial_in)
    clock
    resetGen
    enableGen


main :: IO ()
main = do
  putStrLn "Simulating Blinky Counter"
  print $ List.zip [0..] sim_results