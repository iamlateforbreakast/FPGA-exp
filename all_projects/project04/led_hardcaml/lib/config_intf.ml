(* config_intf.ml *)
module type S = sig
  val clk_fre : int
  val cycle_delay : int
  val ws2812_num : int
  val ws2812_width : int
  val colors : int list
end

module type Config = sig
  module type S = S
end
