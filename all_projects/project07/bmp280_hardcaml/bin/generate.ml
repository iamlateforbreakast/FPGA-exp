(* generate.ml *)
open Hardcaml
open Project07_lib

module My_config = struct
  let clk_fre = 27_000_000
  let uart_fre = 27_000_000
  let baud_rate = 115_200
  let i2c_address = 0x76
  let pattern = [0x21; 0x12; 0x0C; 0x00; 0x12; 0x21]
  let is_simulation = false
end

let () =
  let module MyBmp280 = Top.Make(My_config) in
  let module TopCircuit = Circuit.With_interface(MyBmp280.I)(MyBmp280.O) in
  let scope = Scope.create ~flatten_design:false () in
  (* let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () *)
  let circuit = TopCircuit.create_exn ~name:"top_level" (MyBmp280.create scope) in
  let database = Scope.circuit_database scope in
  (* Generate the circuit *)
  let output_dir = "verilog_out" in
  let _ = Sys.command ("mkdir -p " ^ output_dir) in
  Rtl.output
      ~database
      Verilog
      ~output_mode:(In_directory output_dir)
      circuit
