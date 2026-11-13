(* generate.ml *)
open Hardcaml
open Project09_lib

module Config_nano20k = struct
  let por_bits = 9
  let is_simulation = false
  let normalize_reset x = x (* Nano 20K button is active-high. *)
  let led_width = 6
end

module Config_nano4k = struct
  let por_bits = 9
  let is_simulation = false
  let normalize_reset x = Signal.(~:x) (* Nano 4K buttons are active-low. *)
  let led_width = 1
end

let generate (module X : Config.S) ~output_dir =
  let module MyTop = Top.Make (X) in
  let module TopCircuit = Circuit.With_interface (MyTop.I) (MyTop.O) in
  let scope = Scope.create ~flatten_design:false () in
  let circuit = TopCircuit.create_exn ~name:"top_level" (MyTop.create scope) in
  let database = Scope.circuit_database scope in
  let _ = Sys.command ("mkdir -p " ^ output_dir) in
  Rtl.output ~database Verilog
    ~output_mode:(In_directory output_dir)
    circuit

let () =
  let board = Option.value (Sys.getenv_opt "BOARD") ~default:"nano20k" in
  let output_dir = "verilog_out/" ^ board in
  match board with
  | "nano20k" -> generate (module Config_nano20k : Config.S) ~output_dir
  | "nano4k" -> generate (module Config_nano4k : Config.S) ~output_dir
  | other ->
    failwith
      (Printf.sprintf "Unknown BOARD '%s'; expected nano20k or nano4k" other)
