(* Test_utils.ml — lightweight test helpers for HardCaml simulations *)
open Hardcaml
open Hardcaml_waveterm

(* ------------------------------------------------------------------ *)
(*  Assertion helpers                                                    *)
(* ------------------------------------------------------------------ *)

let total_tests  = ref 0
let failed_tests = ref 0

let pass label =
  incr total_tests;
  Printf.printf "  [PASS] %s\n%!" label

let fail label msg =
  incr total_tests;
  incr failed_tests;
  Printf.printf "  [FAIL] %s — %s\n%!" label msg

let check label cond =
  if cond then pass label else fail label "condition is false"

let check_eq label ~expected ~got =
  if expected = got
  then pass label
  else fail label (Printf.sprintf "expected %d, got %d" expected got)

let check_range label ~lo ~hi v =
  if v >= lo && v <= hi
  then pass label
  else fail label (Printf.sprintf "%d not in [%d, %d]" v lo hi)

let summary () =
  Printf.printf "\n=== %d/%d tests passed ===\n%!"
    (!total_tests - !failed_tests) !total_tests;
  if !failed_tests > 0 then exit 1

(* ------------------------------------------------------------------ *)
(*  Waveform helper — saves a .vcd alongside the test executable        *)
(* ------------------------------------------------------------------ *)

let save_waves ~filename (waves : Waveform.t) =
  Waveform.Serialize.marshall waves filename;
  Printf.printf "  [INFO] waveform written → %s\n%!" filename

(* ------------------------------------------------------------------ *)
(*  Cycle-step helpers                                                   *)
(* ------------------------------------------------------------------ *)

(** Run [sim] for [n] cycles, calling [f sim] after each rising edge. *)
let run_cycles sim ~n ~f =
  for _ = 1 to n do
    Cyclesim.cycle sim;
    f sim
  done

(** Read a port value as a plain int. *)
let port_int (p : Bits.t ref) = Bits.to_int !p

(** Read a single-bit port as bool. *)
let port_bool (p : Bits.t ref) = Bits.to_int !p = 1