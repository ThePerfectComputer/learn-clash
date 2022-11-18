module Counter where
-- module Counter (topEntity, plus, main) where

import Clash.Prelude

-- adds 1 to the provided value
incr :: Signed 8 -> Signed 8
incr a = a + 1

counter = register 0 counter'
counter' = fmap incr counter

-- | 'topEntity' is Clash's equivalent of 'main' in other programming
-- languages. Clash will look for it when compiling 'Example.Project'
-- and translate it to HDL. While polymorphism can be used freely in
-- Clash projects, a 'topEntity' must be monomorphic and must use non-
-- recursive types. Or, to put it hand-wavily, a 'topEntity' must be
-- translatable to a static number of wires.
-- topEntity :: Signal System (Signed 8)
-- topEntity = counter

-- specify signature that sample is the default System domain.
sampleNSystem :: NFDataX a => Int -> Signal System a -> [a]
sampleNSystem a b = sampleN a b
-- t1 = sampleN @System 4 (register 0 (pure (8 :: Signed 8)))

main :: IO ()
main = do
  putStrLn "Simulating Adder"
  -- putStrLn $ show t1

