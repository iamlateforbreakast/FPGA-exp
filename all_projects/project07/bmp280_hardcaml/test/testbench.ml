(* testbench.ml — executable entry point *)
(* All test logic and the DISPLAY_WAVES viewer live in testbench_lib.ml.
   This file exists solely so `dune exec ./testbench.exe` works. *)
let () = Testbench_lib.run ()