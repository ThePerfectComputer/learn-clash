module RS232(topEntity, main) where
import qualified Data.List as List
import Util((+>>.))
import Clash.Prelude
import Clash.Annotations.SynthesisAttributes (Attr(BoolAttr))

data SerializerState = IDLE | START | DATA (Index 8) | PARITY | STOP
  deriving (Generic, Show, Eq, NFDataX)

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

predicatedDeSerialize ::  forall n . KnownNat n =>
                          1 <= n =>
                          SerializerState ->
                          BitVector 8 ->
                          FTDIClockState n ->
                          Bit ->
                          BitVector 8
predicatedDeSerialize serializerState bitVec8In ftdiClock bitIn  = 
  case serializerState of
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
  where captureDeserializerReg :: SerializerState ->
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

        ftdiStateReg :: Signal dom SerializerState
        ftdiStateReg = register IDLE $  ftdiStateReg'
                                    <$> sigBitInFalling
                                    <*> ftdiClock
                                    <*> ftdiStateReg

        ftdiStateReg' :: Bool ->              -- sigBitInIsFalling
                         FTDIClockState n ->  -- ftdiClockState
                         SerializerState ->   -- serializerState
                         SerializerState
        ftdiStateReg' sigBitInIsFalling ftdiClockState serializerState = 
          case serializerState of
            IDLE   | sigBitInIsFalling                      -> START
            START  | ftdiClockAdvancing                     -> DATA 0
            DATA n | ftdiClockAdvancing && (n < maxBound)   -> DATA (n + 1)
                   | ftdiClockAdvancing && (n == maxBound)  -> PARITY
            PARITY | ftdiClockAdvancing                     -> STOP
            STOP   | ftdiClockAdvancing                     -> IDLE
            _                                               -> serializerState
            where ftdiClockAdvancing = isFullCycle ftdiClockState


serial_loopback :: HiddenClockResetEnable dom =>
                   Signal dom (Bit) ->
                   Signal dom (Bit)
serial_loopback serial_in = register 0 serial_in

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
        -- reify maybeVal  = case maybeVal of
        --                     Just val -> val
        --                     Nothing -> 0

-- https://github.com/clash-lang/clash-compiler/blob/master/examples/Blinker.hs#L20
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


main :: IO ()
main = do
  putStrLn "Simulating Blinky Counter"