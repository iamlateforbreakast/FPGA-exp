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
make clean
```

`make` defaults to `BOARD=nano20k`. Generated Verilog is kept separately under
`verilog_out/nano20k/` and `verilog_out/nano4k/` so switching boards cannot
silently reuse stale RTL.

`dune exec ./bin/simulate.exe` runs a small behavioural testbench and opens
the generated `rpi_spi.vcd` waveform. It sends 0x01, 0x02, 0x2A, 0x41, 0x55,
0xAA and prints the pipelined MISO response. Set `BOARD=nano4k` to simulate
the 4K's active-low reset and single LED:

```sh
BOARD=nano4k dune exec ./bin/simulate.exe
```

Running the Pi side
-------------------

Enable SPI on the Pi (`raspi-config` → Interface Options → SPI), install
`python3-spidev`, then:

```sh
python3 raspberry_pi/spi_loopback.py --count 32
python3 raspberry_pi/spi_loopback.py --speed 1000000 --count 256
python3 raspberry_pi/spi_loopback.py --led 0x2a
python3 raspberry_pi/spi_loopback.py --led 0x41   # Nano 4K: bit 0 -> LED on
```

The script primes the pipeline with a `0x00` transfer, then checks that every
response equals the byte sent in the previous iteration and reports a
per-transfer round-trip time. That timing includes Python and `spidev`
overhead, so it is an upper bound on the link latency, not a measurement of
the FPGA alone.

If transfers mismatch, slow the clock down (`--speed 100000`) before
suspecting the design: the slave oversamples SCLK with the 27 MHz system clock
and, like any asynchronous input, needs SCLK comfortably slower than that.

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

Verified on a Tang Nano 4K driven by a Raspberry Pi Zero W, with the bitstream
both loaded to SRAM and written to flash. `spi_loopback.py --count 32` passes,
and `--led 0x01` lights the LED. A 256-byte random-data loopback gave:

| SCLK | Result |
| ---- | ------ |
| 500 kHz | 256/256 |
| 1 MHz (script default) | 256/256 |
| 2 MHz | 256/256 |
| 4 MHz | 88/256 |
| 8 MHz | 2/256 |

**Keep SCLK at or below 2 MHz.** SCLK goes through a two-flop synchronizer and
an edge detector on the 27 MHz clock, so MISO changes about four system
clocks (~150 ns) after each falling SCLK edge. At 4 MHz half an SCLK period is
only 125 ns, so the Pi samples MISO before the new bit is there.

The Nano 20K build has not been tested on hardware.

### If every byte reads back as `0x00`

Check the wiring before the design. On the Pi, `pinctrl` can tell a floating
MISO line from a driven one: enable the pull-up with `pinctrl set 9 pu`, then
run a transfer. If it reads `0xFF`, nothing is driving MISO. If it still reads
`0x00`, the line is driven or tied low. If the FPGA is not the one driving it,
the MISO wire is in the wrong 4K hole or touching GND. Go by the FPGA pin
numbers printed next to the holes; they do not run in order along the header.
With the FPGA disconnected, a jumper from MOSI to MISO on the Pi should echo
every byte within the same transfer.
