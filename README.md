<!-- omit in toc -->
# Simple Starter Project
This starter project contains the scaffolding needed to integrate Clash with the Cabal and Stack build systems. It allows you to use dependencies from [Hackage](https://hackage.haskell.org/) easily.

# Getting Started
This works with ghc 9.0.2

The clash compiler is basically a modified version of ghc designed to allow for first class support of haskell code as circuits.

To run the adder example, or any of the examples for that matter, you first need to buld the ``clash`` compiler. You can build the clash compiler with ``stack build``.

## Simulating the Counter
```bash
stack run clash  -- src/Counter.hs -main-is Counter -o out/Counter
$./out/Counter
```

## Simulating the Blinker
```bash
stack run clash -- src/Blinky.hs -main-is Blinky.main -o out/Blinky
```

## Programming The ULX3S FPGA

```bash
cd ulx3s/
TOP_ENTITY_MODULE=Blinky make
```

The FPGA should now be blinking.

To get the FPGA to count from 0 to 255 cyclically, do:

```bash
cd ulx3s/
TOP_ENTITY_MODULE=BlinkyCount make
```