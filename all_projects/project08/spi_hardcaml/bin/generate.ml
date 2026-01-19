(* main.ml *)
open Hardcaml
open Project8_lib

module My_config = struct
  let file_name = "image.hex"
  let startup_wait = 10_000_000
  let clk_div = 4
  let commands = [ 0xAE; 0x81; 0x7F; 0xA6; 0x20; 0x00; 0xDB; 0x20; 0xD9; 0x22; 0x8D; 0x14; 0xA4; 0XAF ]
  let is_simulation = false
end

let () =
  let module MyScreen = Screen.Make(My_config) in
  let module TopCircuit = Circuit.With_interface(MyScreen.I)(MyScreen.O) in
  let scope = Scope.create ~flatten_design:false () in
  let circuit = TopCircuit.create_exn ~name:"screen" (MyScreen.create scope) in
  let database = Scope.circuit_database scope in
  (* Generate the circuit *)
  let output_dir = "verilog_out" in
  let _ = Sys.command ("mkdir -p " ^ output_dir) in
  Rtl.output
      ~database
      Verilog
      ~output_mode:(In_directory output_dir)
      circuit
