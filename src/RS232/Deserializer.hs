module RS232.Deserializer(deserializer) where
import Clash.Prelude

import Util((+>>.))
import RS232.State(State(..))
import RS232.FtdiClock( FTDIClockState(..),
                        isHalfCycle,
                        isFullCycle,
                        mkFTDIClock)

predicatedDeSerialize ::  forall n . KnownNat n =>
                          1 <= n =>
                          State ->
                          BitVector 8 ->
                          FTDIClockState n ->
                          Bit ->
                          BitVector 8
predicatedDeSerialize deserializerState bitVec8In ftdiClock bitIn  = 
  case deserializerState of
    IDLE -> 0
    START -> 0
    DATA _ | (isHalfCycle ftdiClock) -> bitIn +>>. bitVec8In
           | otherwise -> bitVec8In
    STOP  -> bitVec8In
    PARITY  -> bitVec8In

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
        captureDeserializerReg STOP bitVector  = Just bitVector
        captureDeserializerReg _    _          = Nothing

        sigBitInFalling :: Signal dom Bool
        sigBitInFalling = isFalling 0 sigBitIn

        ftdiClock = mkFTDIClock ftdiClockEn ftdiClockPeriod
          where ftdiClockEn = (/= IDLE) <$> ftdiStateReg

        deserializerReg  :: Signal dom (BitVector 8)
        deserializerReg  = register (0 :: BitVector 8) $  predicatedDeSerialize 
                                                      <$> ftdiStateReg 
                                                      <*> deserializerReg 
                                                      <*> ftdiClock
                                                      <*> sigBitIn

        ftdiStateReg :: Signal dom State
        ftdiStateReg = register IDLE $  ftdiStateReg'
                                    <$> sigBitInFalling
                                    <*> ftdiClock
                                    <*> ftdiStateReg

        ftdiStateReg' :: Bool ->              -- sigBitInIsFalling
                         FTDIClockState n ->  -- ftdiClockState
                         State ->   -- deserializerState
                         State
        ftdiStateReg' sigBitInIsFalling ftdiClockState deserializerState = 
          case deserializerState of
            IDLE   | sigBitInIsFalling                      -> START
            START  | ftdiClockAdvancing                     -> DATA 0
            DATA n | ftdiClockAdvancing && (n < maxBound)   -> DATA (n + 1)
                   | ftdiClockAdvancing && (n == maxBound)  -> PARITY
            PARITY | ftdiClockAdvancing                     -> STOP
            STOP   | ftdiClockAdvancing                     -> IDLE
            _                                               -> deserializerState
            where ftdiClockAdvancing = isFullCycle ftdiClockState