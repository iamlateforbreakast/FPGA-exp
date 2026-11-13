open Hardcaml

module type S = sig
  (** 27 MHz board oscillator. *)
  val clock_freq : int

  (** Fabric/PLL clock.  The memory clock is pll_freq / 4, because the
      controllers use a four-phase fabric clock to place DDR data and the
      SDRAM/HyperRAM clock edges without needing ODDR/IDDR primitives. *)
  val pll_freq : int

  val is_simulation : bool

  (** Active-high reset normalisation for the board. *)
  val normalize_reset : Signal.t -> Signal.t

  (** LED normalisation (the Nano 20K LEDs are active-low, the Nano 4K LED is
      active-high). *)
  val normalize_led : Signal.t -> Signal.t

  val led_width : int

  (** PLL primitive and divider settings.  See bin/generate.ml. *)
  val pll_primitive : [ `RPLL | `PLLVR ]

  val pll_idiv_sel : int
  val pll_fbdiv_sel : int
  val pll_odiv_sel : int
  val gowin_device : string
end
