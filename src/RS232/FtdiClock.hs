module RS232.FtdiClock( FTDIClockState(..),
                        isHalfCycle,
                        isFullCycle,
                        mkFTDIClock) where
import Clash.Prelude

data FTDIClockState hi = NotRunning | Running (Index hi)
  deriving (Generic, Show, Eq, NFDataX)

isHalfCycle :: forall n . KnownNat n =>
               1 <= n =>
               FTDIClockState n -> Bool
isHalfCycle (Running cnt) = cnt == halfMaxVal
  where halfMaxVal        = div (maxBound :: Index n) 2
isHalfCycle NotRunning    = False

isFullCycle :: forall n . KnownNat n =>
               1 <= n =>
               FTDIClockState n -> Bool
isFullCycle (Running cnt) = cnt == (maxBound :: Index n)
isFullCycle NotRunning    = False

mkFTDIClock :: forall n dom . HiddenClockResetEnable dom =>
               (1 <= n) =>
               Signal dom Bool ->
               SNat n ->
               Signal dom (FTDIClockState n)
mkFTDIClock en SNat = sig
  where sig = register NotRunning (sig' <$> en <*> sig)
        sig' :: Bool -> FTDIClockState n -> FTDIClockState n
        sig' False _            = NotRunning
        sig' True NotRunning    = Running 0
        sig' True (Running val) = Running $ satSucc SatWrap val