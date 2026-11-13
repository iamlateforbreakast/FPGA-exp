open Project13_lib

(* The behavioural SDRAM/HyperRAM models and their testbenches live in
   [Sim_models] so that the same code backs both [dune exec ./bin/simulate.exe]
   and these inline tests.  Each test runs the full 4096-word write/read
   sequence and checks the controller's pass flag. *)

let%test "sdram memory test passes" = Sim_models.run_sdram () = `Pass
let%test "hyperram memory test passes" = Sim_models.run_hyperram () = `Pass

(* Debug_spi: trigger a capture of a known probe sequence, then read the
   header and the first trace entries back as an SPI mode-0 master. *)
let%test "debug spi readout" =
  let open Hardcaml in
  let open Base in
  let module D = Debug_spi.Make (struct let trace_depth = 8 end) in
  let module Sim = Cyclesim.With_interface (D.I) (D.O) in
  let sim = Sim.create (D.create (Scope.create ())) in
  let i = Cyclesim.inputs sim and o = Cyclesim.outputs sim in
  let cycle n = for _ = 1 to n do Cyclesim.cycle sim done in
  let header = List.init 16 ~f:(fun k -> 0x10 + k) in
  i.header
  := Bits.concat_msb (List.map header ~f:(fun b -> Bits.of_int ~width:8 b));
  i.cs_n := Bits.vdd;
  i.reset := Bits.vdd;
  cycle 2;
  i.reset := Bits.gnd;
  i.trigger := Bits.vdd;
  for k = 0 to 9 do
    i.probe := Bits.of_int ~width:16 (0xAB00 + k);
    cycle 1;
    i.trigger := Bits.gnd
  done;
  cycle 4;
  i.cs_n := Bits.gnd;
  cycle 8;
  let read_byte () =
    let v = ref 0 in
    for _ = 1 to 8 do
      v := (!v lsl 1) lor Bits.to_int !(o.miso);
      i.sclk := Bits.vdd;
      cycle 8;
      i.sclk := Bits.gnd;
      cycle 8
    done;
    !v
  in
  (* Not List.init: Base's evaluates the function from the last index. *)
  let got = ref [] in
  for _ = 1 to 20 do
    got := read_byte () :: !got
  done;
  let got = List.rev !got in
  (* Entry 0 is the trigger cycle's probe, so the trace is 0xAB00,
     0xAB01, ... *)
  let expected = header @ [ 0xAB; 0x00; 0xAB; 0x01 ] in
  if not (List.equal Int.equal got expected)
  then
    Stdlib.Printf.printf "got %s\n" (String.concat ~sep:" " (List.map got ~f:(Printf.sprintf "%02x")));
  List.equal Int.equal got expected
;;
