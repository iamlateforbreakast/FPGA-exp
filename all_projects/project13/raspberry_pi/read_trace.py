#!/usr/bin/env python3
"""Read project13's Nano 4K debug trace over SPI (lib/debug_spi.ml).

Wiring is the project09 rig: Pi SCLK/MOSI/MISO/CE0 -> Nano 4K pins
39/40/41/42.  Prints the status header, then one line per 81 MHz fabric
clock of the first HyperRAM read transaction.
"""
import spidev

HEADER = 16
DEPTH = 512

spi = spidev.SpiDev()
spi.open(0, 0)
spi.mode = 0
spi.max_speed_hz = 1_000_000
data = spi.xfer2([0] * (HEADER + 2 * DEPTH))
spi.close()

h = data[:HEADER]
print("header", " ".join(f"{b:02x}" for b in h))
if h[0] != 0xA5:
    print("bad magic: check wiring / bitstream")
status, state = h[1], h[2]
addr = (h[3] << 16) | (h[4] << 8) | h[5]
dout = (h[6] << 8) | h[7]
print(f"pass={status & 1} fail={(status >> 1) & 1} trace_done={(status >> 2) & 1} "
      f"test_state={state} addr={addr:#x} dout={dout:#06x}")
print("idx ck cs rwds dq_oe rwds_oe rwds_out dq")
for k in range(DEPTH):
    e = (data[HEADER + 2 * k] << 8) | data[HEADER + 2 * k + 1]
    print(f"{k:3d} {e >> 15 & 1}  {e >> 14 & 1}  {e >> 13 & 1}    {e >> 12 & 1}     "
          f"{e >> 11 & 1}       {e >> 10 & 1}        {e & 0xFF:02x}")
