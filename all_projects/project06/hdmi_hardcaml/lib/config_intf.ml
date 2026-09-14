module type S = sig
  val clk_fre : int
  val h_total : int
  val v_total : int
  val h_res   : int
  val v_res   : int
  val h_sync  : int
  val v_sync  : int
  val h_bporch: int
  val v_bporch: int
  val hs_pol   : bool
  val vs_pol   : bool
  val is_simulation : bool
  val pattern : int list

  (* Board-specific reset button wiring differs in polarity (e.g. the Nano
     20K's reset input is active-high, pulled down when idle; the Nano 4K's
     onboard buttons are active-low, pulled up when idle): normalize the raw
     pin into the active-high signal lib/top.ml expects. See
     bin/generate.ml's Config_nano20k. *)
  val normalize_reset : Hardcaml.Signal.t -> Hardcaml.Signal.t

  (* Board-specific LED polarity: given the internal active-high "lit"
     signal, produce whatever level actually needs to be driven onto the
     physical LED pin(s) for this board. *)
  val normalize_led : Hardcaml.Signal.t -> Hardcaml.Signal.t

  (* Number of onboard LEDs to drive: 6 on the Nano 20K, 1 on the Nano 4K
     (see tangnano4k.cst). Sets the width of Top.O.led and Leds.O.leds. *)
  val led_width : int

  (* Board-specific PLL primitive and parameters, used to derive the serial
     (5x pixel) clock from the 27 MHz oscillator. The Nano 20K's GW2A family
     exposes this as rPLL; the Nano 4K's GW1NS-4 family exposes it as PLLVR
     instead - a different primitive, not just a different DEVICE string on
     the same one. Both share the frequency formula
     CLKOUT = 27MHz * (pll_fbdiv_sel+1) / (pll_idiv_sel+1); pll_odiv_sel only
     needs to keep the internal VCO within the family's legal PLL range. See
     lib/top.ml's rpll/pllvr and bin/generate.ml's Config_nano20k/
     Config_nano4k. *)
  (* Differential output buffer primitive for the TMDS lanes.

     `TLVDS is the hard true-LVDS driver; `ELVDS is Gowin's emulated LVDS -
     two ordinary LVCMOS drivers on the P/N pins of a pair driven in
     anti-phase.

     The Tang Nano 4K needs `ELVDS. Its HDMI pins ARE true-LVDS-capable
     bels, and `TLVDS places, packs and loads without any tool complaining -
     but the monitor never locks onto the result. Sipeed's own reference
     bitstream for this board (TangNano-4K-example/hdmi_720p, built with the
     Gowin IDE) drives these same pins with ELVDS_OBUF and displays
     correctly on the same board, cable and monitor - verified here by
     flashing their prebuilt hdmi.fs as a control. Their clocking is
     identical to ours (PLLVR 27*14/3 = 126MHz, CLKDIV /5 = 25.2MHz), so the
     output buffer is the difference that matters.

     NOTE: apycula refuses ELVDS on a bel its device database marks
     TRUELVDS ("location is a True LVDS pin", gowin_pack.py
     check_elvds_placement) - see this project's Makefile for how that guard
     is relaxed. *)
  val lvds_primitive : [ `TLVDS | `ELVDS ]

  val pll_primitive : [ `RPLL | `PLLVR ]
  val pll_idiv_sel : int
  val pll_fbdiv_sel : int
  val pll_odiv_sel : int
  val gowin_device : string
end

module type Config = sig
  module type S = S
end
