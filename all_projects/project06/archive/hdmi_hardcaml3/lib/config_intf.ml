(* config_intf.ml *)
module type S = sig
  val svo_mode : string
end

module type Config = sig
  module type S = S
end
