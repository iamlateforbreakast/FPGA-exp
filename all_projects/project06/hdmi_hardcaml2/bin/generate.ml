(* generate.ml *)
open Hardcaml
open Project06_lib

module My_config = struct
  let svo_mode = "640x480"
end

let () =
  let module HdmiTop = Top.Make(My_config) in
  let module TopCircuit = Circuit.With_interface(HdmiTop.I)(HdmiTop.O) in
  let scope = Scope.create ~flatten_design:false () in
  let circuit = TopCircuit.create_exn ~name:"hdmi_top" (HdmiTop.create scope) in
  let database = Scope.circuit_database scope in
  (* Generate the circuit *)
  let output_dir = "verilog_out" in
  let _ = Sys.command ("mkdir -p " ^ output_dir) in
  Rtl.output
      ~database
      Verilog
      ~output_mode:(In_directory output_dir)
      circuit
