(* config_intf.ml *)
module type S = sig
  val svo_mode : string
  val svo_frame_rate : int
  val svo_bits_per_pixel : int
  val svo_bits_per_red : int
  val svo_bits_per_green : int
  val svo_bits_per_blue : int
  val svo_bits_per_alpha : int
end

module type Config = sig
  module type S = S
end
