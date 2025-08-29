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
      count : 'a [@bits 6];
    } [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let r = reg_fb spec ~enable:i.enable ~width:6 ~f:(fun d -> (d +:. 1)) in
      { O.count = r }
  
  let circuit_create (scope : Scope.t) = 
    let module CounterCirc = Circuit.With_interface(I)(O) in
    CounterCirc.create_exn ~name:"counter" (create scope)
end

(* A top-level module that instantiates the counter *)
module Top = struct
  module I = struct
    type 'a t = {
      clock_1 : 'a;
      clear_1 : 'a;
      enable_1 : 'a;
    } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      count_1 : 'a [@bits 6];
    } [@@deriving sexp_of, hardcaml]
  end

  let create (scope : Scope.t) (i : _ I.t) =
    (* Instantiate the counter module *)
    let counter_inst = Counter.create scope {clock=i.clock_1; clear=i.clear_1; enable=i.enable_1}
    in
    { O.count_1 = counter_inst.count }
  
  let circuit_create (scope : Scope.t) = 
    let module TopCirc = Circuit.With_interface(I)(O) in
    TopCirc.create_exn ~name:"top" (create scope)
end

let () =
  let scope = Scope.create ~flatten_design:false () in
  let output_dir = "./verilog_output" in
  (* Create the output directory if it doesn't exist *)
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" output_dir) in

  (* Create the circuits for both modules *)
  let counter_circuit = Counter.circuit_create scope in
  let top_circuit = Top.circuit_create scope in

  (* Generate Verilog files for each module *)
  let _ = Rtl.output
    ~output_mode:(In_directory output_dir)
    Verilog
    top_circuit in
  
    Rtl.output
    ~output_mode:(In_directory output_dir)
    Verilog
    counter_circuit
