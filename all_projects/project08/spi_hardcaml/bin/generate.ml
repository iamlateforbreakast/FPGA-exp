(* main.ml *)
open Hardcaml
open Project08_lib

module Common_config = struct
  (* let file_name = "image.hex" *)
  (* let startup_wait = 10_000_000 *)
  let clk_div = 54
  let commands = [
    0xAE; (* Display Off *)
    0xD5; 0x50; (* Clock Divide *)
    0xA8; 0x3F; (* Mux Ratio *)
    0xD3; 0x00; (* Offset *)
    0x40; (* Start Line *)
    0x20; 0x00; (* Memory Mode: Horizontal *)
    0x8D; 0x14; (* Charge Pump *)
    0xA1; (* Segment Remap *)
    0xC8; (* COM Output Scan Direction *)
    0xDA; 0x12; (* COM Pins hardware config *)
    0xD9; 0x22; (* Pre-charge period *)
    0xDB; 0x40; (* VCOMH Deselect *)
    0x32; (* Set pump voltage *)
    0x81; 0xCF; (* Contrast *)
    0xA6; (* Normal Display *)
    0xA4; (* Resume RAM to Display *)
    0xAF; (* Display On *)
  ]

  let is_simulation = false
end

(* Nano 20K reset button is wired active-high (see tangnano20k.cst). *)
module Config_nano20k = struct
  include Common_config
  let normalize_reset x = x
end

(* Nano 4K reset button is wired active-low, pulled up (see tangnano4k.cst). *)
module Config_nano4k = struct
  include Common_config
  let normalize_reset x = Hardcaml.Signal.(~: x)
end

let generate (module X : Config.S) ~output_dir =
  let module MyScreen = Top.Make(X) in
  let module TopCircuit = Circuit.With_interface(MyScreen.I)(MyScreen.O) in
  let scope = Scope.create ~flatten_design:false () in
  let circuit = TopCircuit.create_exn ~name:"top_level" (MyScreen.create scope) in
  let database = Scope.circuit_database scope in
  (* Generate the circuit *)
  let _ = Sys.command ("mkdir -p " ^ output_dir) in
  Rtl.output
      ~database
      Verilog
      ~output_mode:(In_directory output_dir)
      circuit

let () =
  let board = Option.value (Sys.getenv_opt "BOARD") ~default:"nano20k" in
  (* Output is namespaced per board: the generated RTL differs (see
     normalize_reset), so a shared verilog_out/ would go stale silently
     when switching boards without touching any source file. *)
  let output_dir = "verilog_out/" ^ board in
  match board with
  | "nano20k" -> generate (module Config_nano20k : Config.S) ~output_dir
  | "nano4k" -> generate (module Config_nano4k : Config.S) ~output_dir
  | other -> failwith (Printf.sprintf "Unknown BOARD '%s'; expected nano20k or nano4k" other)
