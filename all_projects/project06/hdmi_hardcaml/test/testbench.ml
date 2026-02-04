open Base
open Hardcaml
open Hardcaml_waveterm

module Simulator = Cyclesim.With_interface (Dvi_encoder.I) (Dvi_encoder.O)

let testbench () =
  (* 1. Create the simulation *)
  let sim = Simulator.create (Dvi_encoder.create (Scope.create ())) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  (* 2. Set up waveform recording *)
  let waves, sim = Waveform.create sim in

  (* Helper to run one cycle *)
  let cycle () = Cyclesim.cycle sim in

  (* 3. Simulation Stimulus *)
  inputs.rst_n := Bits.gnd;
  cycle ();
  inputs.rst_n := Bits.vdd;
  inputs.de := Bits.vdd; (* Data Enable active *)

  (* Test a sequence of values *)
  let test_values = [ 0x00; 0xFF; 0x55; 0xAA; 0x01; 0x02 ] in
  List.iter test_values ~f:(fun v ->
    inputs.data := Bits.of_int ~width:8 v;
    cycle ()
  );

  (* Test control characters (DE = low) *)
  inputs.de := Bits.gnd;
  inputs.control := Bits.of_int ~width:2 0b11;
  cycle ();

  waves

(* Run and display *)
let () =
  let waves = testbench () in
  Waveform.print waves
