module type S = sig
  val clk_freq : int
  val uart_freq : int
  val message : string
  val cycle_period : int
  val is_simulation : bool
end
