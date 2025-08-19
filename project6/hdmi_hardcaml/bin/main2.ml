open Hardcaml

(* A simple counter module *)
module Counter = struct
  open Signal
  module I = struct
    type 'a t = {
      clock : 'a;
      clear : 'a;
      enable : 'a;
    } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      count : 'a;
    } [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let r = reg_fb spec ~enable:i.enable ~width:4 (fun d -> d +:. 1) in
    { O.count = r }
  
  let circuit_name = "counter"
  let circuit_create = Circuit.create_exn ~name:circuit_name create
end

(* A top-level module that instantiates the counter *)
module Top = struct
  open Signal
  module I = struct
    type 'a t = {
      clock : 'a;
      clear : 'a;
      enable : 'a;
    } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      count : 'a;
    } [@@deriving sexp_of, hardcaml]
  end

  let create (scope : Scope.t) (i : _ I.t) =
    (* Instantiate the counter module *)
    let counter_inst =
      instantiate
        ~scope
        ~name:"my_counter"
        (module Counter.I)
        (module Counter.O)
        Counter.create
        i
    in
    { O.count = counter_inst.count }
  
  let circuit_name = "top"
  let circuit_create = Circuit.create_exn ~name:circuit_name create
end

let () =
  let output_dir = "./verilog_output" in
  (* Create the output directory if it doesn't exist *)
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" output_dir) in

  (* Create the circuits for both modules *)
  let counter_circuit = Counter.circuit_create in
  let top_circuit = Top.circuit_create in

  (* Generate Verilog files for each module *)
  Rtl.write_verilog
    ~output_mode:Verilog_mode.Verilog_file_per_module
    ~output_path:output_dir
    [ counter_circuit; top_circuit ]
