(* config_intf.ml *)
module type S = sig
  (* Width of the power-on-reset counter. The reset is released once its top
     bit sets, i.e. after 2^(por_bits-1) system clock cycles. Smaller values
     keep behavioural simulation short; hardware uses enough bits to ride out
     configuration. *)
  val por_bits : int

  (* True for simulation builds. Useful for shortening long waits. *)
  val is_simulation : bool

  (* Board-specific reset-button polarity. The Nano 20K's button is
     active-high, so its hardware config passes the pin through unchanged;
     the Nano 4K's onboard button is active-low and is inverted there. *)
  val normalize_reset : Hardcaml.Signal.t -> Hardcaml.Signal.t

  (* Number of onboard LEDs: 6 on the Tang Nano 20K, 1 on the Tang Nano 4K.
     This sets the width of [Top.O.leds]. *)
  val led_width : int
end

module type Config = sig
  module type S = S
end
