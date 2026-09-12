(* config_intf.ml *)
module type S = sig
  val file_name : string
  val startup_wait : int
  val clk_div : int
  val commands : int list
  val is_simulation : bool

  (* SH1106 has 132 columns of RAM but most 1.3-inch 128x64 breakouts only
     wire up the middle 128, so writes must be shifted right by this many
     columns (set via the page/column-address commands) to land on-screen.
     2 is the common default for these modules. *)
  val col_offset : int

  (* Board-specific reset button wiring differs in polarity (e.g. the Nano
     20K's is active-high, the Nano 4K's is active-low): normalize the raw
     pin into the active-high signal the rest of the design expects. *)
  val normalize_reset : Hardcaml.Signal.t -> Hardcaml.Signal.t
end

module type Config = sig
  module type S = S
end
