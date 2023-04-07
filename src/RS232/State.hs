module RS232.State(State(..)) where
import Clash.Prelude

data State = IDLE | START | DATA (Index 8) | PARITY | STOP
  deriving (Generic, Show, Eq, NFDataX)
