(* main.ml *)

open Project3_lib
open Hardcaml

module SimpleCircuit = Circuit.With_interface(Counter.I)(Counter.O)

let circuit = SimpleCircuit.create_exn Counter.create ~name:"counter"

let () = Rtl.print Verilog circuit
