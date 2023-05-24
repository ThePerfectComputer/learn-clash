# Getting Started
This works with ghc 9.0.2

The clash compiler is basically a modified version of ghc designed to allow for first class support of haskell code as circuits.

To run the adder example, or any of the examples for that matter, you first need to buld the ``clash`` compiler. You can build the clash compiler with ``stack build``.

You can find the following top level files in ``./src``
as well as instructions for running them in the top level files
themselves.

# Needed Utilities
 - [ ] LookOnce Result Interface
 - [ ] Fallthrough FIFO

# TODO
 - [ ] turn off derive generics
 - [ ] update commands to use flags that prevent ghc
       intermediate outputs from littering source tree
 - [ ] create simple writing UART example, which I can 
       interactively debug step by step
 - [ ] learn to use trace and jupyter
 - [ ] learn to debug with generic pretty prints
 - [ ] solder header onto ULX3S
 - [ ] debug ULX3S with scope
 - [ ] draw conclusions on feasibility of debugging
       without VCD viewer
 - [ ] make FIFO with good interface
 - [ ] take a look at Clash protocols
 - [ ] add averaging windowed-sample to loopback FTDI SERDES
 - [ ] register comparators for clockAdvancing and 
       sample at half cycle
 - [ ] update FTDI SERDES loopback to use FIFO
 - [ ] build SDRAM controller
 - [ ] write-test SDRAM over UART stream
 - [ ] build user-space PPC CPU