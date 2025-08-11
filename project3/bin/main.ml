(* main.ml *)

open Hardcaml
open Project3_lib.Counter

module CounterCircuit = Circuit.With_interface(I)(O)

let () = print_endline "Hello, World!"
