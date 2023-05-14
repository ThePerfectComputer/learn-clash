module RS232.State(State(..), mkFtdiStateReg) where
import Clash.Prelude
import RS232.FtdiClock(FTDIClockState(..), isFullCycle)

data State = IDLE | START | DATA (Index 8) | STOP1 | STOP2
  deriving (Generic, Show, Eq, NFDataX)

mkFtdiStateReg :: forall n dom . (HiddenClockResetEnable dom, KnownNat n) =>
              (1 <= n) =>
              Signal dom Bool ->
              Signal dom (FTDIClockState n) ->
              Signal dom State
mkFtdiStateReg go ftdiClock = ftdiStateReg
  where ftdiStateReg = register IDLE $  ftdiStateReg' <$> go
                                                      <*> ftdiClock
                                                      <*> ftdiStateReg

ftdiStateReg' ::  forall n . KnownNat n => (1 <= n) =>
                  Bool ->
                  FTDIClockState n ->
                  State ->
                  State
ftdiStateReg' go ftdiClockState deserializerState = 
  case deserializerState of
    IDLE   | go                                     -> START
    START  | ftdiClockAdvancing                     -> DATA 0
    DATA n | ftdiClockAdvancing && (n < maxBound)   -> DATA (n + 1)
           | ftdiClockAdvancing && (n == maxBound)  -> STOP1
    STOP1  | ftdiClockAdvancing                     -> STOP2
    STOP2  | ftdiClockAdvancing                     -> IDLE
    _                                               -> deserializerState
    where ftdiClockAdvancing = isFullCycle ftdiClockState