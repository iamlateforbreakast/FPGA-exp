(* config_intf.ml *)
module type S = sig
  val file_name : string
end

module type Config = sig
  module type S = S
end
