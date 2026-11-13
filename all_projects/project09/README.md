Raspberry Pi to Tang Nano SPI link
==================================

Connect a Raspberry Pi (SPI master) to a Tang Nano 20K or Tang Nano 4K (SPI
slave) and use the link to move bytes between the two boards, drive the
FPGA's LEDs, and measure round-trip latency.

The FPGA implements a mode-0 (CPOL=0, CPHA=0), MSB-first, 8-bit SPI slave:

- MISO returns the byte received in the **previous** transaction. The first
  transaction after reset returns `0x00`, so the link is a one-byte pipeline.
- A received byte with `01` in its top two bits (i.e. `0x40`–`0x7F`) drives
  the on-board LEDs with its low bits: all six on the 20K, the low bit on the
  4K (which has a single LED). Other bytes only drive the loopback.
- `o_irq` goes high when a byte completes and is cleared when CS asserts for
  the next transaction, so the Pi can poll a GPIO instead of guessing when a
  transfer is done.

Two implementations of the slave share that behaviour and the same pins, so
they can be compared on the same board:

| `SPI=` | Module | How it handles SCLK | Tested limit |
| ------ | ------ | ------------------- | ------------ |
| `sclk` (default) | `lib/spi_slave_sclk.ml` | Shift registers clocked by SCLK; only a "byte done" toggle crosses into the 27 MHz domain, for the LEDs and `o_irq` | 15 MHz |
| `oversampled` | `lib/spi_slave_oversampled.ml` | SCLK, CS and MOSI sampled by the 27 MHz clock through two-flop synchronizers, with edges detected there | 2 MHz |

`Top.Make` takes the slave as a parameter (`lib/spi_slave_intf.ml` defines
the interface they share), and `bin/generate.ml` picks one from the `SPI`
environment variable. See [Hardware status](#hardware-status) for why the
oversampled slave tops out at 2 MHz.

Wiring
------

Both boards are 3.3V, but they must share a ground.

| Raspberry Pi 40-pin | Signal | Nano 20K pin | Nano 4K pin |
| ------------------- | ------ | ------------ | ----------- |
| 23 (GPIO11) | SCLK | `i_sclk` = 42 | `i_sclk` = 39 |
| 19 (GPIO10) | MOSI | `i_mosi` = 41 | `i_mosi` = 40 |
| 21 (GPIO9)  | MISO | `o_miso` = 54 | `o_miso` = 41 |
| 24 (GPIO8)  | CE0  | `i_cs_n` = 51 | `i_cs_n` = 42 |
| 22 (GPIO25) | IRQ  | `o_irq` = 56 (optional) | `o_irq` = 43 (optional) |
| 6  | GND | GND | GND |

Keep the ground wire short and connect it before the signal wires. All SPI
pins above are on 3.3V banks, so no level shifting is needed.

On the Nano 4K the reset button is active-low and sits on a 1.8V bank; this
is handled by `tangnano4k.cst` together with the board-specific
`normalize_reset` in `bin/generate.ml`. The 4K's single LED shows bit 0 of the
received byte, so send e.g. `0x41` to turn it on.

Building the bitstream
----------------------

The open-source flow (yosys, nextpnr-himbaechel, gowin_pack) is described in
the repository README.

```sh
podman compose run --rm fpga-exp-dev
cd all_projects/project09/rpi_spi_hardcaml
dune build                 # check the Hardcaml design compiles
make                       # Nano 20K: rpi_spi_hardcaml_nano20k.fs
make BOARD=nano4k          # Nano 4K:  rpi_spi_hardcaml_nano4k.fs
make BOARD=nano4k SPI=oversampled   # rpi_spi_hardcaml_nano4k_oversampled.fs
make clean
```

`make` defaults to `BOARD=nano20k SPI=sclk`. Generated Verilog is kept
separately under `verilog_out/<board>/<spi>/`, and the oversampled bitstreams
get an `_oversampled` suffix, so switching board or slave cannot silently
reuse stale RTL or overwrite the other bitstream.

The Nano 20K `SPI=sclk` build prints `Warning: ABC: execution of command
... failed: return code 134`. ABC (in yosys 0.38) hits an assertion in its
optional `&mfs` pass after LUT mapping has finished, and yosys keeps the
mapped netlist. That netlist passes the full testbench when simulated at gate level
against yosys's Gowin cell models, so the warning can be ignored.

`make sim` simulates the generated Verilog with Icarus Verilog
(`sim/tb_top.v`). Hardcaml's Cyclesim runs every register off a single clock,
so it cannot model a slave clocked by SCLK alongside the 27 MHz logic; the
Verilog testbench drives the two clocks independently. At 1, 2, 4, 8, 10, 16
and 25 MHz it sends 64 random bytes and checks the pipelined MISO response
(including that MISO holds just after each rising edge), that `o_irq` sets
and clears, and that `0x41` / `0x40` switch the LED:

```sh
make sim                   # Nano 20K
make sim BOARD=nano4k      # Nano 4K: active-low reset, single LED
make sim SPI=oversampled   # the oversampled slave
make sim SIMFLAGS=+vcd     # also write tb_top.vcd
```

The `sclk` slave passes at every rate. The `oversampled` slave passes up to
4 MHz and fails from 8 MHz: the simulation has no pad or wire delays, so it
lasts a little longer than on hardware.

Icarus Verilog is not in the `fpga-exp-dev` image. Inside the container,
`sudo apt-get install iverilog` adds it until the container exits.

Running the Pi side
-------------------

Enable SPI on the Pi (`raspi-config` → Interface Options → SPI), install
`python3-spidev`, then:

```sh
python3 raspberry_pi/spi_loopback.py --count 32
python3 raspberry_pi/spi_loopback.py --speed 1000000 --count 256   # 1 MHz
python3 raspberry_pi/spi_loopback.py --led 0x2a
python3 raspberry_pi/spi_loopback.py --led 0x41   # Nano 4K: bit 0 -> LED on
python3 raspberry_pi/spi_loopback.py --burst 4096 --count 50   # throughput
```

The script primes the pipeline with a `0x00` transfer, then checks that every
response equals the byte sent in the previous iteration and reports a
per-transfer round-trip time. That timing includes Python and `spidev`
overhead, so it is an upper bound on the link latency, not a measurement of
the FPGA alone.

The script defaults to 10 MHz, which suits the default `sclk` slave; the
`oversampled` slave needs `--speed 2000000` or lower. If transfers mismatch,
slow the clock down (`--speed 1000000`) before suspecting the design: with the
`sclk` slave, above about 15 MHz the wiring and pad delays, not the logic, are
what fail.

### Burst transfers and throughput

With the default `sclk` slave, CS can stay low for any number of bytes: the
bit counter wraps every eight bits, and each byte of a burst returns the byte
before it (the first returns the last byte of the previous transfer). Every
byte is also decoded as an LED command, as it would be on its own.

`--burst N` sends `--count` transfers of `N` random bytes each, checks every
returned byte, and reports the throughput. It moves bytes in the `0x40`–`0x7F`
LED range out of it, so bursts leave the LEDs alone. `N` can be at most the
spidev driver's `bufsiz` (4096 by default), which the script reads from
`/sys/module/spidev/parameters/bufsiz`.

The `oversampled` slave does not support bursts: it loads its transmit byte
only when CS asserts, so every byte after the first returns `0x00`, and
`--burst` reports those as mismatches.

Measured on the Pi Zero W and Nano 4K, with no mismatches:

| How the Pi sends | 1 MHz | 10 MHz |
| ---------------- | ----- | ------ |
| Raw SCLK rate | 125 kB/s | 1250 kB/s |
| One byte per transfer (the default mode) | 9.4 kB/s | 10.2 kB/s |
| `--burst 4096 --count 50` | 118 kB/s (95%) | 790–862 kB/s (63–69%) |

Each `xfer2()` call costs about 100 µs in Python and the kernel, so with one
byte per transfer that overhead, not SCLK, sets the rate. Bursts spread it
over thousands of bytes; at 10 MHz the driver and the Pi's SPI controller
still leave gaps between bytes. The throughput printed is measured over the
`xfer2()` calls only, not over generating and checking the data.

### Ready LED

A headless Pi Zero W gives no sign of when it is on Wi-Fi and accepting SSH.
`raspberry_pi/ready_led.sh` drives an LED on a Pi GPIO to show this:

- **Blinking:** booting, or not yet on Wi-Fi with `sshd` running.
- **Solid on:** `wlan0` has an IPv4 address and `sshd` is active, so you can
  ssh in.

Wire GPIO17 (header pin 11) through a 330 Ω resistor to the LED's anode (long
leg), and the cathode to GND (pin 9). GPIO17 is clear of the SPI pins and the
IRQ line.

Add this line to the end of `/boot/firmware/config.txt`:

```
dtoverlay=gpio-led,gpio=17,label=ready,trigger=timer
```

The overlay registers the pin as `/sys/class/leds/ready`, and `trigger=timer`
makes the kernel blink it early in boot, before the script starts. Then
install the script and its systemd unit, and reboot:

```sh
sudo install -m 755 raspberry_pi/ready_led.sh /usr/local/bin/
sudo install -m 644 raspberry_pi/ready-led.service /etc/systemd/system/
sudo systemctl enable ready-led
sudo reboot
```

The script checks the state every 2 seconds and logs each change, which
`journalctl -t ready-led` shows. On the test Pi Zero W, the LED goes solid
about 3 minutes after power-on. To use a different pin, change `gpio=` in the
overlay line. The script does not need to change.

Hardware status
---------------

Both slaves are verified on a Tang Nano 4K driven by a Raspberry Pi Zero W,
wired through a ribbon cable, a breakout board and jumper wires. A 1024-byte
random-data loopback of single-byte transfers, with each bitstream loaded to
SRAM in turn, gave:

| SCLK | `SPI=sclk` | `SPI=oversampled` |
| ---- | ---------- | ----------------- |
| 1 MHz | 1024/1024 | 1024/1024 |
| 2 MHz | 1024/1024 | 1024/1024 |
| 4 MHz | 1024/1024 | 393/1024 |
| 8 MHz | 1024/1024 | 10/1024 |
| 10 MHz (script default) | 1024/1024 | 12/1024 |
| 15.4 MHz (16 MHz requested) | 1024/1024 | 3/1024 |
| 20 MHz | 42/1024 | 11/1024 |
| 25 MHz | 12/1024 | 12/1024 |

The Pi Zero W's SPI clock is its 400 MHz core clock divided by an even
number, so some requested rates come out lower, like 16 MHz above. `o_irq`
reads high after each byte; its clearing on CS is checked in simulation only.

**With `SPI=sclk`, keep SCLK at or below 15 MHz; 10 MHz leaves margin.** The
design itself passes at 25 MHz in simulation, and nextpnr puts the SCLK
domain's internal limit well above that, so the failures above 15 MHz come
from pad delays and the wiring.

**With `SPI=oversampled`, keep SCLK at or below 2 MHz** (pass `--speed
2000000` to `spi_loopback.py`). SCLK goes through a two-flop synchronizer and
an edge detector on the 27 MHz clock, so MISO changes about four system
clocks (~150 ns) after each falling SCLK edge. At 4 MHz half an SCLK period is
only 125 ns, so the Pi samples MISO before the new bit is there.

The Nano 20K builds of both slaves have not been tested on hardware.

### If every byte reads back as `0x00`

Check the wiring before the design. On the Pi, `pinctrl` can tell a floating
MISO line from a driven one: enable the pull-up with `pinctrl set 9 pu`, then
run a transfer. If it reads `0xFF`, nothing is driving MISO. If it still reads
`0x00`, the line is driven or tied low. If the FPGA is not the one driving it,
the MISO wire is in the wrong 4K hole or touching GND. Go by the FPGA pin
numbers printed next to the holes; they do not run in order along the header.
With the FPGA disconnected, a jumper from MOSI to MISO on the Pi should echo
every byte within the same transfer.
