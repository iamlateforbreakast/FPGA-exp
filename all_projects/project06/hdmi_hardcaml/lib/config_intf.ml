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
  val h_total : int
  val is_simulation : bool
  val pattern : int list
end

module type Config = sig
  module type S = S
end
