(* config_intf.ml *)
module type S = sig
  val clk_fre : string
  val ws2812_num : int
  val ws2812_width : int
end

module type Config = sig
  module type S = S
end
