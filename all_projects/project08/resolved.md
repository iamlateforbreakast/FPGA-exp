# Nano 4K OLED bring-up: what was actually wrong

A record of the faults fixed while getting the SH1106 SPI OLED working on the Tang Nano 4K.
There were nine of them, in four different layers — host tooling, constraints, protocol, and
synthesis — and they masked each other, which is why "the screen is black" stayed true for so
long while each individual fix appeared to change nothing.

The command sequence itself was correct the whole time. It matches the known-good RP2040
reference driver (`MCU-exp/workspace/RPI2040/examples/oled_1in3_sh1106_spi`) byte for byte.

---

## 1. The project did not build

`bin/generate.ml` had `file_name` and `startup_wait` commented out while `Config.S` still
required them. Nothing could be generated until they were restored.

## 2. Flashing did not work at all

Three separate host-side problems, all in the container setup rather than the design:

| Symptom | Cause | Fix |
| --- | --- | --- |
| `-4 usb_open failed` | container UID did not match the device owner | `userns_mode: keep-id` |
| SELinux AVC denial | container blocked from the USB device node | `security_opt: label=disable` |
| `-6 ftdi_usb_reset failed` | kernel `ftdi_sio` driver holding the interface | unbind it on the host |

These live in `docker-compose.yml` under `fpga-exp-flash`.

## 3. The panel is an SH1106, not an SSD1306

These 1.3" modules are widely sold as SSD1306-compatible and are not:

- charge pump is `0xAD` / `0x8B`, **not** SSD1306's `0x8D` / `0x14` — get this wrong and the
  panel stays dark no matter what else you send;
- no horizontal addressing mode, so every page needs its own `0xB0|page` plus column-nibble
  commands before its 128 data bytes;
- 132 columns of RAM behind a 128-pixel window, so the visible area starts at column 2
  (`col_offset`).

## 4. Page-setup bytes were generated but never transmitted

`data_valid` into `screen_spi` listed `SEND_DATA` and `SEND_CMD` but omitted `PAGE_SETUP`, so
the page and column commands were selected onto `current_data` and then never clocked out.

## 5. `i_reset` had the wrong IO standard, and it stopped the design dead

Pin 15 is in **Bank3, VCCIO 1.8V**. The constraint declared `LVCMOS33`, whose VIH minimum
(~2.0V) is above the 1.8V the internal pull-up can reach. The released button therefore read
LOW or marginal — and because the 4K's button is active-low the design inverts it, so a
misread idle level became an *asserted* reset that parked every register.

Being marginal rather than flatly wrong, it presented intermittently. Fixed by declaring
`LVCMOS18`, where 1.8V sits comfortably above the ~1.17V VIH.

Note `IO_TYPE` is **bank-wide**: parking an `LVCMOS33` output on another Bank3 pin while this
one is `LVCMOS18` produces an `IO_TYPE conflict` from `gowin_pack`.

## 6. The state register had no initialiser

An attempt to avoid the marginal reset pin above (`normalize_reset _pin = gnd`) removed the
only thing that initialised the state register, leaving the FSM free to power up in an unused
encoding where no branch matches, nothing is ever assigned, and it stays there forever.

Replaced with a power-on reset — a saturating counter that asserts `por` for the first 256
cycles after configuration — plus a self-heal branch for out-of-range encodings. The button is
ORed in on top rather than relied upon.

## 7. A 0.74s start-up delay made every hardware observation ambiguous

`startup_wait` was `10_000_000` cycles — 0.37s per phase. Any capture shorter than the delay
is indistinguishable from a design that never starts. Reduced to `270_000` (10ms at 27MHz),
matching the reference driver.

## 8. yosys re-encoded the FSM to one-hot, and it froze on power-up

The hardest one, and the reason several earlier theories seemed to half-fit.

Start-up relies on flops powering up to zero being `INIT`. Gowin's GSR guarantees the zeros,
and under Hardcaml's **binary** encoding all-zeros genuinely is `INIT`. Under **one-hot** it is
not: `INIT` becomes `0000001`, all-zeros decodes to no state at all, every next-state term
evaluates false, and the machine never moves.

yosys will make that substitution. A synchronous clear is just another term in the register's
input mux, so `FSM_EXTRACT` absorbs it into the transition tree — it reports

```
found reset state: 3'000 (guessed from mux tree)
```

— and `FSM_RECODE` then re-encodes to one-hot.

**It was only sometimes free to.** yosys skips a register that reaches a module port:

```
Not marking top_level._39 as FSM state register:
    Register is connected to module port.
```

Four debug outputs exporting the state number had been satisfying that condition. They were
**accidentally load-bearing**: deleting purely observational ports broke a working design,
which is exactly the kind of change nobody suspects.

Fixed by pinning the encoding in the Makefile (`synth_gowin` has no `-nofsm` flag):

```make
yosys -q -p "read_verilog $(VERILOG_FILES); setattr -set fsm_encoding \"none\" w:*; synth_gowin ..."
```

An asynchronous reset also fixes the hardware — it puts the reset on a real flop pin, outside
the transition logic, correct under any encoding. It was rejected because Cyclesim does not
model a reset generated *inside* the design, so the whole POR window becomes invisible in
simulation: the testbench showed `dc` going high at cycle 13, mid-POR, with the machine racing
into the data phase before sending a single command. Correct silicon, useless test. Pinning the
encoding keeps hardware and simulation in agreement.

## 9. The init sequence was clocked into a panel held in reset

`X.startup_wait` is referenced only by the older, unused `lib/screen.ml` — never by `top.ml`.
Nothing gated the command sequence, so it began 9.5µs after power-on while `oled_rst_cnt` held
RES low for another 19ms. Every byte of the first init was discarded.

Added a `panel_ready` gate at roughly twice the RES pulse, so commands arrive after the panel
is out of reset and settled.

## 10. The init sequence was replayed every frame

End-of-frame returned to `INIT`, re-sending all 22 command bytes — including display-off and
display-on — about 29 times a second. The reference driver initialises once and then only
streams data. Now loops to `PAGE_SETUP`.

---

## Theories that were wrong

Recorded so they are not rediscovered. Both were written into source comments as fact and had
to be removed.

**"Pins 39/40 are Bank3 / 1.8V."** A misreading of Sipeed's pinmap — the rings are yellow
(Bank1, 3.3V), not green. This produced a confident but false explanation for the panel
ignoring SCLK/SDIN, and a pin move that caused a real second problem.

**"The freeze is caused by pin 44 being GCLKC_0."** Plausible — 41/42/43/44 are global-clock
input pins — and it survived a while because moving any one signal did change the outcome. It
was false. The freeze reproduced with SCLK/SDIN on 16/17 and with the OLED physically
disconnected, ruling out both pin location and panel loading.

**"The OLED module is dead."** It was not, and was proved working by the RP2040 reference
firmware.

---

## Method notes

These cost more time than any individual bug.

**A missing ground wire invalidates everything.** A Pico↔FPGA ground came loose during
rewiring, and every input then floated and read a flat zero — indistinguishable from "the
target is driving nothing". Several conclusions drawn in that window were simply wrong,
including one that an ADC read 0V from a known-good 3.3V rail.

**Every capture needs a control channel.** A free-running counter bit on a spare pin proves the
probe, the ground and the target's clock are all good *within that same measurement*. Without
one, "the signal is dead" and "my probe is dead" are indistinguishable.

**Change one variable per flash.** The comparison "runs on 16/17, freezes on 39/40" moved the
pins *and* the debug outputs at once, and pointed firmly at the wrong cause. Holding the RTL
constant and moving only the pins took one build and settled it.

**Reproduce the control before theorising about the difference.** Rebuilding the known-good
configuration and confirming it still ran turned "removing four output ports breaks the design"
from an absurd claim into a reproducible fact worth explaining.

**Cross-check independent channels.** SCLK measured 249.9kHz against a designed 250kHz, and DC
edge counts independently predicted the frame rate to within a percent — before *and* after the
init-once change, where the predicted rate shifted from 93.4 to 95.4 edges/200ms and the
measurements moved 94 → 96. Two unrelated signals agreeing is much stronger evidence than
either alone.

**Stale labels are worse than no labels.** The probe firmware's channel names were hardcoded
and outlived two rewirings, so its output looked authoritative while two columns were
transposed. Rename them whenever the wiring changes.
