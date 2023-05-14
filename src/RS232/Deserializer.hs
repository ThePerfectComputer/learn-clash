module RS232.Deserializer(deserializer) where
import Clash.Prelude

import Util((+>>.))
import RS232.State(State(..), mkFtdiStateReg)
import RS232.FtdiClock(FTDIClockState(..), isHalfCycle, mkFTDIClock)

predicatedDeSerialize ::  forall n . KnownNat n =>
                          1 <= n =>
                          State ->
                          BitVector 8 ->
                          FTDIClockState n ->
                          Bit ->
                          BitVector 8
predicatedDeSerialize deserializerState bitVec8In ftdiClock bitIn  = 
  case deserializerState of
    IDLE                              -> 0
    START                             -> 0
    DATA _ | (isHalfCycle ftdiClock)  -> bitIn +>>. bitVec8In
           | otherwise                -> bitVec8In
    STOP1                             -> bitVec8In
    STOP2                             -> bitVec8In

deserializer :: forall n dom . (HiddenClockResetEnable dom, KnownNat n) =>
             (1 <= n) =>
             Signal dom Bit ->
             SNat n ->
             Signal dom (Maybe (BitVector 8))
deserializer sigBitIn ftdiClockPeriod =   captureDeserializerReg
                                      <$> ftdiStateReg
                                      <*> deserializerReg
  where captureDeserializerReg :: State ->
                               BitVector 8 ->
                               Maybe (BitVector 8)
        captureDeserializerReg STOP2 bitVector = Just bitVector
        captureDeserializerReg _    _          = Nothing

        trigger :: Signal dom Bool
        trigger = isFalling 0 sigBitIn

        ftdiClock = mkFTDIClock ftdiClockEn ftdiClockPeriod
          where ftdiClockEn = (/= IDLE) <$> ftdiStateReg

        ftdiStateReg = mkFtdiStateReg trigger ftdiClock

        deserializerReg  :: Signal dom (BitVector 8)
        deserializerReg  = register (0 :: BitVector 8) $  predicatedDeSerialize 
                                                      <$> ftdiStateReg 
                                                      <*> deserializerReg 
                                                      <*> ftdiClock
                                                      <*> sigBitIn