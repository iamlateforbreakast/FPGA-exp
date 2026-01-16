(* generate.ml *)
open Hardcaml
open Project05_lib

module My_config = struct
  let clk_fre = 27_000_000
  let uart_fre = 1_000_000
  let message = "Hello world!\n"
  let cycle_period = 10
end

let () =
  let module MyUart = Uart.Make(My_config) in
  let module TopCircuit = Circuit.With_interface(MyUart.I)(MyUart.O) in
  let scope = Scope.create ~flatten_design:false () in
  (* let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () *)
  let circuit = TopCircuit.create_exn ~name:"uart" (MyUart.create scope) in
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
