(* main.ml *)
open Hardcaml
open Project8_lib

module My_config = struct
  let file_name = "image.hex"
end

module My_Screen = Screen.Make(My_config)

let create (scope: Scope.t) (input: _ I.t)=
  (* Instantiate the My_screen module with the required inputs *)
  My_screen.create scope input

let () =
  let module TopCircuit = Circuit.With_interface(MyScreen.I)(MyScreen.O) in
  let scope = Scope.create ~flatten_design:false () in
  let circuit = TopCircuit.create_exn ~name:"top" (create scope) in
  let database = Scope.circuit_database scope in
  (* Generate the circuit *)
  let output_dir = "verilog_out" in
  let _ = Sys.command ("mkdir -p " ^ output_dir) in
  Rtl.output
      ~database
      Verilog
      ~output_mode:(In_directory output_dir)
      circuit
