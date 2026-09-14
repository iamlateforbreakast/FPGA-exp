#!/usr/bin/env python3
"""gowin_pack wrapper that permits ELVDS_OBUF on true-LVDS-capable pins.

Why this exists
---------------
The Tang Nano 4K's HDMI pins (27/28, 29/30, 31/32, 34/35) are marked
TRUELVDS in apycula's device database for the GW1NS-4, and apycula's
`Device.check_elvds_placement` refuses outright to place an ELVDS_OBUF on
any such pin:

    Exception: X37Y14/IOBA (...) cannot be placed - location is a True LVDS pin

That guard is stricter than the silicon. The pins really are true-LVDS
*capable*, but this board's HDMI connector wants the emulated-LVDS driver,
and the hardware accepts it: Sipeed's own reference bitstream for this board
(TangNano-4K-example/hdmi_720p/hdmi.fs, built with the Gowin IDE) drives
these exact pins with ELVDS_OBUF and displays correctly. Verified on
hardware here - TLVDS_OBUF places, packs and loads with no tool warning at
all but never gets a monitor to lock, while ELVDS_OBUF through this wrapper
produces a correct picture on the same board, cable and monitor.

Only the true-LVDS refusal is dropped. The P/N ordering check (P must be
IOBA, N must be IOBB) is kept, since getting that wrong really would produce
an invalid bitstream.

Upstream as of apycula 0.33 (and current git master) still has the strict
check, so this wrapper is needed until that changes. If a future apycula
allows ELVDS on these pins, delete this file and call gowin_pack directly
from the Makefile.
"""
import sys

from apycula import gowin_pack as gp


def check_elvds_placement(self, bel):
    cfg = self.chipdb.get_io_diff_cfg(bel.x, bel.y, bel.idx_str)
    if not cfg:
        raise Exception(
            f"X{bel.x}Y{bel.y}/IOB{bel.idx_str} ({bel.cell.name}) "
            "cannot be placed - location is not a LVDS pin")
    if cfg.positive != (bel.cell.parms.get('DIFF') == 'P'):
        raise Exception(
            f"X{bel.x}Y{bel.y}/IOB{bel.idx_str} ({bel.cell.name}) "
            "cannot be placed - pin P must be IOBA, pin N must be IOBB")


gp.Device.check_elvds_placement = check_elvds_placement

if __name__ == '__main__':
    sys.exit(gp.main())
