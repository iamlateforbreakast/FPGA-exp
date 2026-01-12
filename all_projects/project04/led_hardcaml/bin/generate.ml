(* generate.ml *)
open Hardcaml
open Project04_lib

module My_config = struct
  let startup_wait = 10_000_000
  let clk_div = 4
  let commands = [ 0xAE; 0x81; 0x7F; 0xA6; 0x20; 0x00; 0xDB; 0x20; 0xD9; 0x22; 0x8D; 0x14; 0xA4; 0XAF ]
end

let () =
  let module MyLed = Led.Make(My_config) in
  let module TopCircuit = Circuit.With_interface(MyLed.I)(MyLed.O) in
  let scope = Scope.create ~flatten_design:false () in
  (* let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () *)
  let circuit = TopCircuit.create_exn ~name:"led" (MyLed.create scope) in
  (* let create = Top.hierarchical ~build_mode:Simulation scope in *)
  let database = Scope.circuit_database scope in
  (* Generate the circuit *)
  let output_dir = "verilog_out" in
  let _ = Sys.command ("mkdir -p " ^ output_dir) in
  Rtl.output
      ~database
      Verilog
      ~output_mode:(In_directory output_dir)
      circuit
