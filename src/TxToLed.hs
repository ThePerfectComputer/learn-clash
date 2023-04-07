module TxToLed(topEntity) where
import RS232.Deserializer(deserializer)
import Clash.Prelude

reifiedDeserializer :: forall n dom . (HiddenClockResetEnable dom, KnownNat n) =>
             (1 <= n) =>
             Signal dom Bit ->
             SNat n ->
             Signal dom (BitVector 8)
reifiedDeserializer sigBitIn ftdiClockPeriod = val
  where val = register 0 $ val' <$> val <*> deserializerResult
        val' (currVal :: BitVector 8) (maybeNextVal :: Maybe (BitVector 8)) =
          case maybeNextVal of
            Just someVal -> someVal
            Nothing -> currVal
        deserializerResult = deserializer sigBitIn ftdiClockPeriod

{-# ANN topEntity
   (Synthesize{
    t_name = "top",
    t_inputs = [PortName "clk_25mhz",
                PortName "ftdi_txd"],
    t_output =  PortName "led" }) #-}
{-# NOINLINE topEntity #-}
topEntity :: Clock System ->
             Signal System (Bit) ->
             Signal System (BitVector 8)
topEntity clock serial_in =
  exposeClockResetEnable
    (reifiedDeserializer serial_in $ SNat @2604)
    clock
    resetGen
    enableGen