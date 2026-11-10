#!/usr/bin/env python3
"""Loopback test for the Tang Nano 20K SPI slave (project09).

The FPGA returns the byte it received in the previous transaction, so each
transfer is checked against the value sent one iteration earlier. Bytes with
01 in their top two bits also drive the six on-board LEDs.

Enable SPI on the Pi first (raspi-config -> Interface Options -> SPI), then:

    sudo apt install python3-spidev
    python3 spi_loopback.py --count 32
    python3 spi_loopback.py --led 0x2a
"""

import argparse
import sys
import time

try:
    import spidev
except ImportError:
    sys.exit("python3-spidev is not installed (sudo apt install python3-spidev)")


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--bus", type=int, default=0)
    parser.add_argument("--device", type=int, default=0)
    parser.add_argument("--speed", type=int, default=1_000_000,
                        help="SPI clock in Hz (default: 1 MHz)")
    parser.add_argument("--count", type=int, default=16,
                        help="number of loopback bytes (default: 16)")
    parser.add_argument("--led", type=lambda v: int(v, 0),
                        help="only set the LEDs to this 6-bit value and exit")
    return parser.parse_args()


def main():
    args = parse_args()

    spi = spidev.SpiDev()
    spi.open(args.bus, args.device)
    spi.max_speed_hz = args.speed
    spi.mode = 0
    spi.bits_per_word = 8
    spi.cshigh = False

    try:
        # A byte with 01 in the top bits updates the LEDs.
        if args.led is not None:
            spi.xfer2([0x40 | (args.led & 0x3F)])
            print(f"LEDs set to 0x{args.led & 0x3F:02X}")
            return 0

        # Prime the pipeline: this byte is what the first checked transfer
        # should see echoed back, regardless of what the FPGA saw before.
        spi.xfer2([0x00])

        failures = 0
        previous = 0
        for n in range(args.count):
            value = n & 0xFF
            start = time.perf_counter_ns()
            received = spi.xfer2([value])[0]
            elapsed_ns = time.perf_counter_ns() - start

            ok = received == previous
            failures += not ok
            print(f"tx=0x{value:02X} rx=0x{received:02X} "
                  f"expected=0x{previous:02X} "
                  f"{'ok' if ok else 'MISMATCH'} ({elapsed_ns} ns)")
            previous = value
    finally:
        spi.close()

    if failures:
        print(f"{failures} transfer(s) mismatched", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
