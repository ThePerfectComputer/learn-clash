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
to the FPGA LED's, run the following, making sure to change
the path to you ULX3X's FTDI device as necessary:

```bash
cd ulx3s/
TOP_ENTITY_MODULE=TxToLed make
# two stop bits and no parity bits
stty -f /dev/tty.usbserial-K00027 -cstopb -parenb
screen /dev/tty.usbserial-K00027 9600
```

Note that you may need to change the actual tty device
according to your system. I'm not sure if screen defaults
to parity bits and or stop bits - at sufficiently high BAUD,
this may become important - will investigate later.

Eventually - screen's behavior won't matter so much as I'll be
using pySerial...

# Needed Utilities
What if I just use FIFOs everywhere? Is the latency too much to handle?
In general, you should take a latency insensitive approach to designing...

The design should be correct regardless of latency, except when
working with physical interfaces.

I also need to indicate that a module is ready for input.

You could also use put/get like BS, but that could require data only
staying on the line for a single cycle. This is not ideal, especially, if
the getter isn't immediately available to get in a given cycle.

Perhaps make an interface which disallows the device from proceeding to the next
computation until it sees an ack. Pipelines do this with ready.

If data must be dropped, we can specify if we should show the newest
or oldest value.

 - [ ] LookOnce Result Interface
 - [ ] Fallthrough FIFO

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