module type S = sig
  val clk_fre : int
  val baud_rate : int
  val message : string
  val cycle_period : int
  val is_simulation : bool
end

module type Config = sig
  module type S = S
end