# Project 13 — SDRAM on the Tang Nano 20K, HyperRAM on the Tang Nano 4K

This project implements two Hardcaml memory controllers:

| Board | Memory | Controller | Top-level module | Bitstream |
| ----- | ------ | ---------- | ---------------- | --------- |
| Tang Nano 20K (`GW2AR-LV18QN88C8/I7`) | 64 Mbit (8 MiB) SDRAM, 32-bit bus, embedded in the GW2AR-18 | `lib/sdram.ml` | `lib/top_sdram.ml` | `memory_hardcaml_nano20k.fs` |
| Tang Nano 4K (`GW1NSR-LV4CQN48PC7/I6`) | 64 Mbit (8 MiB) HyperRAM, 8-bit bus, embedded in the GW1NS-4 | `lib/hyperram.ml` | `lib/top_hyperram.ml` | `memory_hardcaml_nano4k.fs` |

Both designs run a power-on self-test: they write an address-dependent
pattern to all 4096 words, then read every word back (so an address-decoding
fault shows up as a mismatch), then repeat with the pattern inverted (so
data left in the RAM by an earlier run cannot fake a pass; the HyperRAM keeps
its contents across FPGA reloads), and light an LED on success or failure.
The controller logic, PLL wrapper, test sequencer and I/O buffers
are all written in Hardcaml; the generated Verilog is only post-processed to
turn the bidirectional DQ/RWDS ports into `inout` (the same trick
`project07` uses for its I2C SDA pin).

## Build

The build uses the open-source flow:

```
dune exec ./bin/generate.exe   ->  yosys (synth_gowin)
                               ->  nextpnr-himbaechel
                               ->  gowin_pack
                               ->  memory_hardcaml_<board>.fs
```

```sh
cd all_projects/project13

# Tang Nano 20K SDRAM (default)
make BOARD=nano20k

# Tang Nano 4K HyperRAM
make BOARD=nano4k

# Rebuild after changing Hardcaml sources
make clean
```

`make` requires `dune`, `yosys`, `nextpnr-himbaechel` and `gowin_pack`
(Apycula) on `PATH`, plus the OCaml packages `hardcaml`, `ppx_hardcaml` and
`ppx_jane`.

## Simulation and tests

The controllers are split into a board-independent `*_system` module and a
board-specific `top_*` wrapper.  Simulation instantiates the system directly,
so no PLL or IOBUF primitives are needed and ordinary `Cyclesim` works.

```sh
# Full 4096-word SDRAM test against the behavioural SDRAM model
dune exec ./bin/simulate.exe

# Full 4096-word HyperRAM test against the behavioural HyperRAM model
BOARD=nano4k dune exec ./bin/simulate.exe

# Both of the above as dune inline tests
dune runtest
```

The SDRAM model decodes `ACTIVE`/`READ`/`WRITE`/`REFRESH`/`MRS` at each CK
rising edge, stores writes, and returns read data after the controller's
CL=2 latency.  The HyperRAM model follows the HyperBus protocol rather than
the controller's assumptions: it drives RWDS during the CA to request 1x or
2x latency (chosen pseudo-randomly per transaction), expects data in CK
clock 3 + L·m as measured on hardware, runs linear bursts while CK keeps
going, honours the RWDS write mask, writes garbage for any data clock the
controller is not driving, and delays what the controller sees by one fabric
clock to stand in for the pad round trip.  `test/testbench.ml` also checks
the SPI trace readout.  Set `DEBUG=1` to print every CA/data transfer.

## Memory clocking

The PLL produces an 81 MHz fabric clock from the 27 MHz board oscillator.
Each memory clock is generated in fabric logic from a two-bit phase counter,
which avoids `ODDR`/`IDDR` primitives and keeps the controllers simulatable.
The resulting memory clock is 81 MHz / 4 = 20.25 MHz.

SDRAM:

```
phase:  0    1    2    3
CK:     0    1    1    0
launch: ^              ^
sample:       ^         ^
```

Commands and write data are launched at phase 0 and read data is sampled at
phase 2 (CK high centre).

HyperRAM (one CK clock runs from phase 1 to the following phase 0):

```
phase:     1    2    3    0
CK:        0    1    1    0          registered, so glitch-free
DQ out:   rising    falling          CK edges centred in each byte
DQ in:              ^rise     ^fall  end of phase 3 / of the next phase 1
```

CK is registered and launched one phase after the data, so each edge sits in
the middle of its byte.  An earlier edge-aligned version (CK decoded
combinationally from the phase counter, changing on the same fabric edge as
DQ) made the HyperRAM latch half-changed CA bytes, so it never answered
reads.  The read capture points allow for the pad round trip (CK out, tCKD,
DQ in), which the debug trace shows is under one fabric clock (12.3 ns).

HyperRAM latency, measured with the debug trace: the CA is CK clocks 1–3 and
the latency count starts at CA clock 3, so data starts in clock 3 + L (1x) or
3 + 2L (2x), with L = 3 set in CR0.  RWDS during the CA selects 1x or 2x;
the controller samples it inside the CA (in clock 3).

Neither controller uses burst mode: SDRAM uses the mode register value
`0x020` (BL=1, sequential, CL=2), and every read or write is a single
auto-precharge access.  This is intentionally simple; the sequencer test is
the interesting part, not peak bandwidth.

### Addressing

- **SDRAM:** 21-bit 32-bit-word address covering 8 MiB.
  `bank = addr[20:19]`, `row = addr[18:8]`, `column = addr[7:0]`.
- **HyperRAM:** 22-bit 16-bit-word address covering 8 MiB.
  The CA carries `addr[21:3]` in the upper address field and `addr[2:0]` in
  the lower three bits.

## Pin / pseudo-pin summary

### Tang Nano 20K SDRAM

`tangnano20k.cst` only constrains the clock, the reset button and the six
on-board LEDs.  The SDRAM is connected through nextpnr's SIP pseudo-pins and
needs no `IO_LOC` entries:

```
IO_sdram_dq[31:0]   inout
O_sdram_addr[10:0]  output
O_sdram_ba[1:0]     output
O_sdram_dqm[3:0]    output
O_sdram_clk         output
O_sdram_cke         output
O_sdram_cs_n        output
O_sdram_ras_n       output
O_sdram_cas_n       output
O_sdram_wen_n       output
```

The LED bank is constrained as 3.3 V (`LVCMOS33`), matching the known-working
`nand2mario/sdram-tang-nano-20k` reference design.  Do **not** add
`BANK_VCCIO=1.8` to the LED entries: the embedded SDRAM pseudo-pins share the
bank and `gowin_pack` will otherwise fail with an `IO_TYPE and BANK_VCCIO
conflict`.

### Tang Nano 4K HyperRAM

`tangnano4k.cst` constrains the clock (pin 45), the active-low reset button
(pin 15, 1.8 V bank), the single active-high LED (pin 10) and the SPI debug
readout on header pins 39 (SCLK), 40 (MOSI, unused), 41 (MISO) and 42 (CS#),
which are the same 3.3 V pins project09 wires to its Raspberry Pi.  nextpnr models
the HyperRAM pseudo-pins as two-entry vectors, even though the board has one
x8 device:

```
IO_hpram_dq[7:0]     inout
IO_hpram_rwds[1:0]   inout   (RWDS[0] used; RWDS[1] driven high-Z by a TBUF)
O_hpram_ck[1:0]      output  (both bits carry the same CK)
O_hpram_ck_n[1:0]    output
O_hpram_cs_n[1:0]    output
O_hpram_reset_n[1:0] output
```

Declaring these as scalars makes nextpnr report `Unconstrained
IO:O_hpram_reset_n_OBUF_O`; the 2-bit vector form is required.

## LED meanings

Tang Nano 20K (`leds[5:0]`, active-low, inverted by the config):

| LED | Meaning |
| --- | ------- |
| `leds[0]` | heartbeat |
| `leds[1]` | test passed |
| `leds[2]` | test failed |
| `leds[3]` | controller ready |
| `leds[4]` | controller busy |
| `leds[5]` | test active (not pass/fail) |

Tang Nano 4K (`O_led`, active-high):

- solid on: test passed
- mostly off, N slow blinks then a pause: read-back mismatch.  N = k + 4 when
  the word read back is the one expected at `addr + k` (k in −3..3), and
  N = 8 when it matches none of those
- mostly on, N short dark gaps then a long on period: still running (hung).
  N is the test FSM state + 1 (1 = `WAIT_INIT` … 6 = `READ`, 7 = `WAIT_READ`)
- the BL702 debugger's activity LED also flashes; it is not part of the design

The SPI debug readout below is far more informative than the LED.

## Nano 4K debug trace over SPI

The 4K has no UART to the debugger, so `lib/debug_spi.ml` adds an on-chip
logic analyser.  It records 512 fabric clocks (128 CK clocks) of the first
HyperRAM read transaction, starting at CS# falling: CK, CS#, RWDS in, DQ/RWDS
output enables, RWDS out and DQ (as seen at the pad).  A host reads it as an
SPI mode-0 master, together with a status header (pass/fail, test FSM
state, current address, last word read).  With the project09 Raspberry Pi
rig wired to pins 39–42:

```sh
scp raspberry_pi/read_trace.py thomas@192.168.0.35:~
ssh thomas@192.168.0.35 python3 read_trace.py
```

The trigger is `in_read` (feeding `trigger`) in `lib/top_hyperram.ml`.  Point it at a
different transaction (e.g. the write of one address) to trace that
instead.

## Source layout

```
lib/config_intf.ml       Board/clock configuration signature
lib/config.ml            Re-export of Config_intf.S
lib/pll.ml               Gowin rPLL (20K) / PLLVR (4K) wrapper
lib/memory_test.ml       Shared 4096-word write-all/read-all pass/fail sequencer
lib/sdram.ml             SDRAM controller
lib/sdram_system.ml      SDRAM controller + test, no board I/O
lib/top_sdram.ml         20K top: PLL, POR, DQ IOBUFs, LEDs
lib/hyperram.ml          HyperRAM controller
lib/hyperram_system.ml   HyperRAM controller + test, LED status codes, no board I/O
lib/debug_spi.ml         On-chip trace buffer read out over SPI (4K debug)
lib/top_hyperram.ml      4K top: PLLVR, POR, DQ/RWDS IOBUFs, vector pseudo-pins, trace
lib/sim_models.ml        Behavioural SDRAM/HyperRAM models and testbenches
bin/generate.ml          Emits Verilog and rewrites the inout ports
bin/simulate.ml          Command-line simulation entry point
test/testbench.ml        dune inline tests for both controllers
tangnano20k.cst          20K clock/reset/LED constraints
tangnano4k.cst           4K clock/reset/LED/debug-SPI constraints
raspberry_pi/read_trace.py  Pi-side reader for the 4K debug trace
```

## Current status and limitations

- Both bitstreams build end-to-end with yosys → nextpnr → `gowin_pack`.
- Both controllers pass the full 4096-word behavioural simulation.
- Nano 4K: the two-round write-all/read-all test passes on hardware,
  5 out of 5 reloads (checked over the SPI readout, 2026-11-13).  The
  first-word captures sit on the last trace sample of each byte window, so
  the read margin is roughly the pad round trip; if reads ever turn
  marginal, try capturing one phase earlier.
- Nano 20K: untested on hardware.  It shares `memory_test.ml`, so the
  two-round test is new there too; it passes in simulation only.
- yosys 0.38 in `fpga-exp-dev` hits an ABC `&mfs` assertion (`Gia_ManInsertMfs`)
  on the 4K build.  It is reported as a warning only: ABC9 has already written
  the mapped netlist before `&mfs` runs, so the output is valid, just without
  that optimisation.
- There is no SDC yet, so nextpnr only checks the 27 MHz `--freq`.  Its
  per-clock estimates for the 81 MHz domain (2026-11-13) are about 110–150 MHz
  on the 4K and about 93 MHz on the 20K.  The 20K margin is thin, and the
  81 MHz domain is not explicitly constrained.
- The SDRAM controller performs one auto-refresh every 303 CK cycles
  (≈15 µs at 20.25 MHz) and uses conservative timings (at least 2 CK cycles for
  `tRCD`/`tRP`/`tRFC`/`tMRD`, and `tWR + tRP` for writes).
- Only the first 4096 words are tested; the full 8 MiB address space is
  decoded but not exercised.
- The HyperRAM RWDS[1] pseudo-pin is deliberately left high-Z; only the x8
  device's single RWDS line is used.
