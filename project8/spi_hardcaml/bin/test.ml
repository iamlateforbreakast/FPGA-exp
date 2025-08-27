(* Try.ml *)
open Hardcaml

module Try = struct
  module I = struct
    type 'a t =
      { i_clk : 'a
      ; i_inc : 'a
      ; i_reset: 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { o_count : 'a [@bits 1]
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Init | Inc | Reset
    [@@deriving sexp_of, compare, enumerate]
  end

  let create (_scope : Scope.t)(i: _ I.t) =
    let open Signal in
    let open Always in
    
    let reg_sync_spec = Reg_spec.create ~clock:i.i_clk ~clear:gnd () in
    let state = State_machine.create (module States) ~enable:vdd reg_sync_spec in
    let counter = reg_fb reg_sync_spec ~width:1 ~enable:vdd ~f:(fun d -> mux2 (state.current ==: Inc.t)(d +:. 1)(d)) in
    let reset = Always.Variable.wire ~default:gnd in
   compile  [ state.switch
      [(Init,[if_ i.i_inc [state.set_next Inc;] 
               [state.set_next Init;]
            ]);
       (Inc,
        [if_ i.i_inc
          [ state.set_next Inc;]
          [if_ i.i_reset [state.set_next Reset][ state.set_next Init; ]]
        ]);
       (Reset,
        [ reset <-- vdd; state.set_next Init;])]];
    {O.o_count = counter}
end

let () =
  let scope = Scope.create ~flatten_design:false () in
  let module TryCircuit = Circuit.With_interface(Try.I)(Try.O) in
  let circuit = TryCircuit.create_exn ~name:"try" (Try.create scope) in
  let output_mode = Rtl.Output_mode.To_file("try.v") in
  Rtl.output ~output_mode Verilog circuit