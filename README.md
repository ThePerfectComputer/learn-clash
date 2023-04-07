<!-- omit in toc -->
# Getting Started
This works with ghc 9.0.2

The clash compiler is basically a modified version of ghc designed to allow for first class support of haskell code as circuits.

To run the adder example, or any of the examples for that matter, you first need to buld the ``clash`` compiler. You can build the clash compiler with ``stack build``.

# Simulating
## Counter
```bash
stack run clash  -- src/Counter.hs -main-is Counter -o out/Counter
$./out/Counter
```

## Blinker
```bash
stack run clash -- src/Blinky.hs -main-is Blinky.main -o out/Blinky
```

# Programming The ULX3S FPGA

## Blinky

```bash
cd ulx3s/
TOP_ENTITY_MODULE=Blinky make
```

The FPGA should now be blinking.

## Counter
To get the FPGA to count from 0 to 255 cyclically, do:

```bash
cd ulx3s/
TOP_ENTITY_MODULE=BlinkyCount make
```

## Serial over USB
To type a character and see its ASCII representation displayed
to the FPGA LED's, do:

```bash
cd ulx3s/
TOP_ENTITY_MODULE=TxToLed make
screen /dev/tty.usbserial-K00027 9600
```

Note that you may need to change the actual tty device
according to your system. I'm not sure if screen defaults
to parity bits and or stop bits - at sufficiently high BAUD,
this may become important - will investigate later.

Eventually - screen's behavior won't matter so much as I'll be
using pySerial...

# TODO
 - [ ] make serializer
 - [ ] make loopback
 - [ ] take a look at Clash protocols
 - [ ] make FIFO with good interface
 - [ ] add averaging windowed-sample to loopback FTDI SERDES
 - [ ] register comparators for clockAdvancing and 
       sample at half cycle
 - [ ] update FTDI SERDES loopback to use FIFO
 - [ ] build SDRAM controller
 - [ ] write-test SDRAM over UART stream
 - [ ] build user-space PPC CPU