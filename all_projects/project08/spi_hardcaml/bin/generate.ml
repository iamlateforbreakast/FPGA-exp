(* main.ml *)
open Hardcaml
open Project08_lib

module My_config = struct
  let file_name = "image.hex"
  let startup_wait = 10_000_000
  let clk_div = 4
  let commands = [
    0xAE; (* Display Off *)
    0xD5; 0x80; (* Clock Divide *)
    0xA8; 0x3F; (* Mux Ratio *)
    0xD3; 0x00; (* Offset *)
    0x40; (* Start Line *)
    0x8D; 0x14; (* Charge Pump *)
    0x20; 0x00; (* Memory Mode: Horizontal *)
    0xA1; (* Segment Remap *)
    0xC8; (* COM Output Scan Direction *)
    0xDA; 0x12; (* COM Pins hardware config *)
    0x81; 0xCF; (* Contrast *)
    0xD9; 0xF1; (* Pre-charge *)
    0xDB; 0x40; (* VCOMH Deselect *)
    0xA4; (* Resume RAM to Display *)
    0xA6; (* Normal Display *)
    0xAF; (* Display On *)
  ]
    
  let is_simulation = false
end

let () =
  let module MyScreen = Top.Make(My_config) in
  let module TopCircuit = Circuit.With_interface(MyScreen.I)(MyScreen.O) in
  let scope = Scope.create ~flatten_design:false () in
  let circuit = TopCircuit.create_exn ~name:"top_level" (MyScreen.create scope) in
  let database = Scope.circuit_database scope in
  (* Generate the circuit *)
  let output_dir = "verilog_out" in
  let _ = Sys.command ("mkdir -p " ^ output_dir) in
  Rtl.output
      ~database
      Verilog
      ~output_mode:(In_directory output_dir)
      circuit
