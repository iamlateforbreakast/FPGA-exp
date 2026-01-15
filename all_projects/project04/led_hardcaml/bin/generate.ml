(* generate.ml *)
open Hardcaml
open Project04_lib

module My_config = struct
  let clk_fre = 27_000_000
  let cycle_delay = 27_000_000
  let ws2812_num = 0
  let ws2812_width = 24
  let colors = [ 0xFF0000; 0x00FF00; 0x0000FF ]  (* Red, Green, Blue *)
  let is_simulation = false
end

let () =
  let module MyWs2812 = Top.Make(My_config) in
  let module TopCircuit = Circuit.With_interface(MyWs2812.I)(MyWs2812.O) in
  let scope = Scope.create ~flatten_design:false () in
  (* let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () *)
  let circuit = TopCircuit.create_exn ~name:"top_level" (MyWs2812.create scope) in
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
