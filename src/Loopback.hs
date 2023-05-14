module Loopback(topEntity) where
import RS232.Deserializer(deserializer)
import RS232.Serializer(serializer, PipeOut(dataOut))
import Clash.Prelude
import Data.Maybe (isJust, fromMaybe)

clkPeriod = SNat @2604

reifiedDeserializer :: forall n dom . (HiddenClockResetEnable dom, KnownNat n) =>
             (1 <= n) =>
             Signal dom Bit ->
             SNat n ->
             Signal dom (BitVector 8)
reifiedDeserializer sigBitIn ftdiClockPeriod = val
  where val = regEn 0 
                    (isJust <$> deserializerResult) 
                    (fromMaybe <$> 0 <*> deserializerResult)
        deserializerResult = deserializer sigBitIn ftdiClockPeriod

loopback :: forall n dom . (HiddenClockResetEnable dom, KnownNat n) =>
             (1 <= n) =>
             Signal dom Bit ->
             SNat n ->
             Signal dom Bit
loopback sigBitIn ftdiClockPeriod = dataOut <$> serializer deserializerResult clkPeriod
  where deserializerResult = deserializer sigBitIn ftdiClockPeriod

{-# ANN topEntity
   (Synthesize{
    t_name = "top",
    t_inputs = [PortName "clk_25mhz",
                PortName "ftdi_txd"],
    t_output =  PortProduct "" [PortName "led", PortName "ftdi_rxd"] 
    }) #-}
{-# NOINLINE topEntity #-}
topEntity :: Clock System ->
             Signal System Bit ->
             (Signal System (BitVector 8),
              Signal System Bit)
topEntity clock serial_in =
  exposeClockResetEnable
    (reifiedDeserializer serial_in clkPeriod
    ,loopback serial_in clkPeriod)
    clock
    resetGen
    enableGen