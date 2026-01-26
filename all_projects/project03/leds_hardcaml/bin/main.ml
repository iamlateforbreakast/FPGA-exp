(* main.ml *)

open Project3_lib.Counter
open Hardcaml

module SimpleCircuit = Circuit.With_interface(I)(O)

let circuit = SimpleCircuit.create_exn create ~name:"counter"

let output_mode = Rtl.Output_mode.To_file("counter.v")

let () = Rtl.output ~output_mode Verilog circuit
