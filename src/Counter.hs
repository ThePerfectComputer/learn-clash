-- | $stack run clash  -- src/Counter.hs -main-is Counter -o out/Counter
-- | $./out/Counter
module Counter (topEntity, main) where

import Clash.Prelude

-- adds 1 to the provided value
incr :: Num a => a -> a
incr a = a + 1

-- | haskell is lazy and allows for infinite list such as
-- >>> numbers1 = 1 : map (+1) numbers1
-- >>> take 3 numbers1
-- [1,2,3]
-- A register is basically an infinite list where a value
-- from the list is sampled each cycle
counter :: HiddenClockResetEnable dom => Signal dom (Signed 8)
counter = c
  where c = register 0 (incr c)

-- | 'topEntity' is Clash's equivalent of 'main' in other programming
-- languages. Clash will look for it when compiling 'Example.Project'
-- and translate it to HDL. While polymorphism can be used freely in
-- Clash projects, a 'topEntity' must be monomorphic and must use non-
-- recursive types. Or, to put it hand-wavily, a 'topEntity' must be
-- translatable to a static number of wires.
topEntity :: HiddenClockResetEnable System => Signal System (Signed 8)
topEntity = counter


-- we specify that we are simulating in the System domain
-- I would like to see if there's a better way to do this withing
-- the type signature for sim_results itself
sim_results = sampleN @System 4 counter

main :: IO ()
main = do
  putStrLn "Simulating Counter"
  putStrLn $ show sim_results
