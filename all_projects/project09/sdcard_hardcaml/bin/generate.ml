(* main.ml *)
open Hardcaml
open Project09_lib

module My_config = struct

end

let () =
  let module MySdCard = Sdcard.Make(My_config) in
  let module TopCircuit = Circuit.With_interface(MySdcard.I)(MySdcard.O) in
  let scope = Scope.create ~flatten_design:false () in
  let circuit = TopCircuit.create_exn ~name:"screen" (MySdcard.create scope) in
  let database = Scope.circuit_database scope in
  (* Generate the circuit *)
  let output_dir = "verilog_out" in
  let _ = Sys.command ("mkdir -p " ^ output_dir) in
  Rtl.output
      ~database
      Verilog
      ~output_mode:(In_directory output_dir)
      circuit
