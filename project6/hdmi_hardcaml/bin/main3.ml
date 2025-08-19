open Hardcaml
open Signal

(* Define the ALU module (same as before) *)
module Alu = struct
  module I = struct
    type 'a t = {
      a : 'a;
      b : 'a;
      op : 'a;
    } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      result : 'a;
    } [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (i : _ I.t) =
    let a, b, op = i.a, i.b, i.op in
    let result =
      match_
        op
        [
          ( of_int ~width:2 0, a +: b ); (* Add *)
          ( of_int ~width:2 1, a -: b ); (* Subtract *)
          ( of_int ~width:2 2, a &: b ); (* And *)
          ( of_int ~width:2 3, a |: b ); (* Or *)
        ]
    in
    { O.result }
end

(* Define the top-level CPU module (same as before) *)
module Cpu = struct
  module I = struct
    type 'a t = {
      operand_a : 'a;
      operand_b : 'a;
      operation : 'a;
    } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      cpu_result : 'a;
    } [@@deriving sexp_of, hardcaml]
  end

  let create (scope : Scope.t) (i : _ I.t) =
    (* Instantiate the ALU sub-module, which automatically gets added to the database *)
    let alu_instantiation =
      instantiate
        ~scope
        ~name:"my_alu"
        (module Alu.I)
        (module Alu.O)
        Alu.create
        { a = i.operand_a; b = i.operand_b; op = i.operation }
    in
    { O.cpu_result = alu_instantiation.result }
end

let () =
  let output_dir = "./database_verilog" in
  (* Create the output directory if it doesn't exist *)
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" output_dir) in

  (* Create the top-level circuit and let Hardcaml build the database automatically *)
  let database =
    Circuit_database.create_exn
      ~top_level_name:"cpu"
      (module Cpu.I)
      (module Cpu.O)
      Cpu.create
  in

  (* Generate a separate Verilog file for each module in the database *)
  Rtl.write_verilog
    ~output_mode:Verilog_mode.Verilog_file_per_module
    ~output_path:output_dir
    database
