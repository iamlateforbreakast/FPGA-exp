(* config_intf.ml *)
module type Config = sig
  val file_name : string
  val startup_wait : int
end
