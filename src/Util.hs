module Util((.<<+)) where

import Clash.Prelude
import Clash.Sized.Internal.BitVector(shiftL#, replaceBit#)

-- import Clash.Sized.BitVector(shiftL, replaceBit)

infixr 4 .<<+
-- | Shift in a bit from the LSB side of a 'BitVector'. Equal to left shifting
-- the 'BitVector' by one and replacing the LSB with the bit to be shifted in.
--
-- >>> 0b1111_0000 .<<+ 0 :: BitVector 8
-- 0b1110_0000
-- >>> 0b1111_0000 .<<+ 1 :: BitVector 8
-- 0b1110_0001
--
(.<<+) :: forall n. KnownNat n => BitVector n -> Bit -> BitVector n
bv .<<+ b = replaceBit# (shiftL# bv 1) 0 b
