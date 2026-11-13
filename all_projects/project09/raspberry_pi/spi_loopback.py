#!/usr/bin/env python3
"""Loopback test for the Tang Nano 20K SPI slave (project09).

The FPGA returns the byte it received in the previous transaction, so each
transfer is checked against the value sent one iteration earlier. Bytes with
01 in their top two bits also drive the six on-board LEDs.

With --burst N, each transfer instead sends N random bytes under one chip
select and the script reports the throughput. Within a burst each byte returns
the one before it. Only the default SPI=sclk slave supports bursts; the
SPI=oversampled one returns 0x00 after the first byte.

Enable SPI on the Pi first (raspi-config -> Interface Options -> SPI), then:

    sudo apt install python3-spidev
    python3 spi_loopback.py --count 32
    python3 spi_loopback.py --led 0x2a
    python3 spi_loopback.py --burst 4096 --count 50
"""

import argparse
import random
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
    parser.add_argument("--speed", type=int, default=10_000_000,
                        help="SPI clock in Hz (default: 10 MHz)")
    parser.add_argument("--count", type=int, default=16,
                        help="number of transfers (default: 16)")
    parser.add_argument("--burst", type=int,
                        help="send this many random bytes per transfer and "
                             "report the throughput")
    parser.add_argument("--led", type=lambda v: int(v, 0),
                        help="only set the LEDs to this 6-bit value and exit")
    return parser.parse_args()


def spidev_bufsiz():
    """Largest transfer the spidev driver accepts (its bufsiz parameter)."""
    try:
        with open("/sys/module/spidev/parameters/bufsiz") as f:
            return int(f.read())
    except OSError:
        return 4096


def no_led(value):
    """Move bytes out of the 0x40-0x7F LED range so bursts leave the LEDs."""
    return value ^ 0x80 if value & 0xC0 == 0x40 else value


def run_bursts(spi, args):
    """Send --count bursts of --burst bytes; return the mismatch count."""
    failures = 0
    previous = 0
    busy_ns = 0
    for n in range(args.count):
        tx = [no_led(random.randrange(256)) for _ in range(args.burst)]
        start = time.perf_counter_ns()
        rx = spi.xfer2(list(tx))  # xfer2 overwrites its argument
        busy_ns += time.perf_counter_ns() - start

        expected = [previous] + tx[:-1]
        bad = [i for i, (r, e) in enumerate(zip(rx, expected)) if r != e]
        if bad:
            i = bad[0]
            print(f"burst {n}: {len(bad)} mismatch(es), first at byte {i}: "
                  f"rx=0x{rx[i]:02X} expected=0x{expected[i]:02X}")
        failures += len(bad)
        previous = tx[-1]

    total = args.count * args.burst
    rate = total / (busy_ns / 1e9)
    print(f"{args.count} burst(s) x {args.burst} bytes = {total} bytes, "
          f"{failures} mismatch(es)")
    print(f"{rate / 1e3:.1f} kB/s while transferring, "
          f"{100 * rate * 8 / args.speed:.0f}% of the requested "
          f"{args.speed / 1e6:g} MHz SCLK")
    return failures


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

        if args.burst is not None:
            limit = spidev_bufsiz()
            if not 1 <= args.burst <= limit:
                print(f"--burst must be between 1 and {limit} (spidev bufsiz)",
                      file=sys.stderr)
                return 2

        # Prime the pipeline: this byte is what the first checked transfer
        # should see echoed back, regardless of what the FPGA saw before.
        spi.xfer2([0x00])

        if args.burst is not None:
            failures = run_bursts(spi, args)
            return 1 if failures else 0

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
