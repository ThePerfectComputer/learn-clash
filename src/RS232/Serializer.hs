module RS232.Serializer(serializer, PipeOut(dataOut, busy)) where
import Clash.Prelude

import RS232.State(State(..), mkFtdiStateReg)
import RS232.FtdiClock(mkFTDIClock)
import Data.Maybe (isJust, fromMaybe)
data PipeOut = PipeOut { dataOut :: Bit, busy :: Bool}
  deriving (Generic, Show, Eq, NFDataX)

serializer ::  forall n dom . (HiddenClockResetEnable dom, KnownNat n) =>
              (1 <= n) =>
              Signal dom (Maybe (BitVector 8)) ->
              SNat n ->
              Signal dom PipeOut
serializer maybeByteIn ftdiClockPeriod =
  out
  where
    out = register initVal $ out' 
                          <$> ftdiStateReg 
                          <*> sampledByte

    initVal = PipeOut{dataOut = 1, busy = False}

    out' :: State -> BitVector 8 -> PipeOut
    out' IDLE _               = PipeOut{dataOut = 1, busy = False}
    out' START _              = PipeOut{dataOut = 0, busy = False}
    out' (DATA n) sampledByte = PipeOut{dataOut = sampledByte ! n,
                                        busy = True}
    out' _        _           = PipeOut{dataOut = 1, busy = False}

    sampleTrigger = (&&) <$> (isJust <$> maybeByteIn)
                   <*> fmap (== IDLE)  ftdiStateReg

    sampledByte = regEn 0 sampleTrigger resolvedByteIn
      where resolvedByteIn = fromMaybe <$> 0 <*> maybeByteIn

    ftdiStateReg = mkFtdiStateReg sampleTrigger ftdiClock

    ftdiClock = mkFTDIClock ftdiClockEn ftdiClockPeriod
      where ftdiClockEn = (/= IDLE) <$> ftdiStateReg