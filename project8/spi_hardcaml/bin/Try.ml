(* Try.ml *)
open Hardcaml

module Try = struct
  module I = struct

  module O = struct

  let create (scope : Scope.t)(i: I.t) =
  {
  }
end

let () =
  let module TryCircuit = Circuit.With_interface(Try.I)(Try.O) in
  let circuit = TryCircuit.create_exn Test.create ~name:"try" in
  let output_mode = Rtl.Output_mode.To_file("try.v") in
  Rtl.output ~output_mode Verilog circuit