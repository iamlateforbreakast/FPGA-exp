(* config_intf.ml *)
module type S = sig
  val clk_fre : int

  val pattern : int list
  val is_simulation: bool
end

module type Config = sig
  module type S = S
end
