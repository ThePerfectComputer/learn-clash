-- | ```bash
-- cd ulx3s/
-- TOP_ENTITY_MODULE=UARTTx make
-- # two stop bits and no parity bits
-- stty -f /dev/tty.usbserial-K00027 -cstopb -parenb
-- screen /dev/tty.usbserial-K00027 9600
-- ```
-- You should now see "abcd" being repeatedly written to
-- the screen.
module UARTTx(topEntity, main) where
import RS232.Deserializer(deserializer)
import RS232.Serializer(serializer, PipeOut(dataOut, busy))
import Clash.Prelude
import Data.Maybe (isJust, fromMaybe)
import Data.Char

clkDivideVal = SNat @2604
clkPeriod = SNat @25_000_000

tx :: forall n n1 dom . (HiddenClockResetEnable dom, KnownNat n, KnownNat n1) =>
            1 <= n =>
            1 <= n1 =>
            SNat n -> SNat n1 ->
             (Signal dom (BitVector 8),
              Signal dom Bit)
tx clkPeriodArg clkDivideValArg = (fromMaybe <$> 0 <*> holdVal, dataOut <$> pipeOut)
  where 
    pipeOut = serializer val clkDivideValArg
    val = lookupTable <$> (riseEvery clkPeriodArg) <*> slowCounter
    holdVal = lookupTable <$> (pure True) <*> slowCounter
    slowCounter = regEn
                0 
                (riseEvery clkPeriodArg) $ 
                register 
                  (0 :: Index 4) $ 
                  (satSucc SatWrap) <$> slowCounter
    lookupTable :: Bool -> Index 4 -> Maybe (BitVector 8)
    lookupTable True n = Just . fromIntegral $ selected_character
      where selected_character = (ord 'a') + (fromIntegral n)
    lookupTable False _ = Nothing

{-# ANN topEntity
   (Synthesize{
    t_name = "top",
    t_inputs = [PortName "clk_25mhz",
                PortName "ftdi_txd"],
    t_output =  PortProduct "" [PortName "led", PortName "ftdi_rxd"] 
    }) #-}
{-# NOINLINE topEntity #-}
topEntity :: Clock System ->
            --  Signal System Bit ->
             (Signal System (BitVector 8),
              Signal System Bit)
topEntity clock =
  exposeClockResetEnable
    (tx clkPeriod clkDivideVal)
    clock
    resetGen
    enableGen

sim_results = sampleN @System 64 $ bundle (tx (SNat @1) (SNat @4))

main = do
  putStrLn "SImulating UARTTx"
  print sim_results