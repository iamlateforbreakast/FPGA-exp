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
      { o_count : 'a [@bits 6]
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t = Init | Inc | Reset
    [@@deriving sexp_of, compare, enumerate]
  end

  let create (scope : Scope.t)(i: I.t) =
    let open Signal in
    let open Try in
    let state = State_machine.create (module States) ~enable:vdd spec in
    let main = [ states.switch
      [Init,[]];
      [Inc,[]];
      [Reset,[]]]
    {O.o_count = counter.value }
end

let () =
  let module TryCircuit = Circuit.With_interface(Try.I)(Try.O) in
  let circuit = TryCircuit.create_exn Test.create ~name:"try" in
  let output_mode = Rtl.Output_mode.To_file("try.v") in
  Rtl.output ~output_mode Verilog circuit