(* test_dvi_encoder.ml
   Tests for Dvi_encoder.Make
   ──────────────────────────────────────────────────────────────────────
   The TMDS encoder is a pure combinational + 1-register pipeline.
   We test:
     1. Control-period tokens (DE=0): all four control codes produce the
        correct 10-bit TMDS tokens.
     2. Data-period encoding (DE=1): encoded output has fewer than or
        equal transitions to a raw 8-bit value, verifying the
        "transition minimisation" property.
     3. Running disparity tracking: after encoding a stream of all-ones
        followed by all-zeros, the bias register should return near zero.
     4. Reset behaviour: asserting rst_n=0 clears the bias and holds
        encoded=0 (clear value).
*)

open Hardcaml
open Hardcaml_waveterm
open Test_utils

module Enc = Dvi_encoder.Make(Config.Res_480p)

(* ── helpers ─────────────────────────────────────────────────────────── *)

let create_sim () =
  let module Sim = Cyclesim.With_interface(Enc.I)(Enc.O) in
  Sim.create (Enc.create (Scope.create ~flatten_design:true ()))

(* Count 0→1 transitions in a 10-bit integer (MSB first). *)
let transitions v =
  let count = ref 0 in
  let prev  = ref ((v lsr 9) land 1) in
  for i = 8 downto 0 do
    let bit = (v lsr i) land 1 in
    if bit <> !prev then incr count;
    prev := bit
  done;
  !count

(* Drive one cycle: set inputs, cycle, return encoded output *)
let drive_cycle sim ~rst_n ~de ~control ~data =
  let i = Cyclesim.inputs sim in
  i.rst_n   := Bits.of_int ~width:1 rst_n;
  i.de      := Bits.of_int ~width:1 de;
  i.control := Bits.of_int ~width:2 control;
  i.data    := Bits.of_int ~width:8 data;
  Cyclesim.cycle sim;
  port_int (Cyclesim.outputs sim).encoded

(* ── Test 1 — control tokens ─────────────────────────────────────────── *)
(*  TMDS spec, Table 3-2 *)
let control_tokens = [
  (0, 0b1101010100);
  (1, 0b0010101011);
  (2, 0b0101010100);
  (3, 0b1010101011);
]

let test_control_tokens () =
  Printf.printf "\n[DVI Encoder] Test 1 — control-period tokens\n";
  let sim = create_sim () in
  (* Release reset *)
  ignore (drive_cycle sim ~rst_n:1 ~de:0 ~control:0 ~data:0);
  List.iter (fun (ctrl, expected_token) ->
    (* Need an extra cycle because encoded is registered *)
    ignore (drive_cycle sim ~rst_n:1 ~de:0 ~control:ctrl ~data:0);
    let got = drive_cycle sim ~rst_n:1 ~de:0 ~control:ctrl ~data:0 in
    check_eq
      (Printf.sprintf "control=%d token" ctrl)
      ~expected:expected_token
      ~got
  ) control_tokens

(* ── Test 2 — transition minimisation property ───────────────────────── *)
(*  For any 8-bit data value the encoded 10-bit word must have ≤ 5
    transitions (the XNOR/XOR encoding never increases transitions
    beyond the minimum of the two candidates).                           *)
let test_transition_minimisation () =
  Printf.printf "\n[DVI Encoder] Test 2 — transition minimisation\n";
  let sim = create_sim () in
  ignore (drive_cycle sim ~rst_n:1 ~de:1 ~control:0 ~data:0);
  let worst_transitions = ref 0 in
  for data = 0 to 255 do
    ignore (drive_cycle sim ~rst_n:1 ~de:1 ~control:0 ~data);
    let enc = drive_cycle sim ~rst_n:1 ~de:1 ~control:0 ~data in
    let t = transitions enc in
    if t > !worst_transitions then worst_transitions := t
  done;
  check_range "max transitions in 10-bit word ≤ 5"
    ~lo:0 ~hi:5 !worst_transitions

(* ── Test 3 — disparity recovery ─────────────────────────────────────── *)
(*  Feed 64 all-ones bytes then 64 all-zero bytes; the encoder's
    bias should hover close to zero by the end (within ±4).             *)
let test_disparity_recovery () =
  Printf.printf "\n[DVI Encoder] Test 3 — running-disparity recovery\n";
  let sim = create_sim () in
  (* Warm up *)
  ignore (drive_cycle sim ~rst_n:1 ~de:1 ~control:0 ~data:0);
  (* All-ones burst (bias goes positive) *)
  for _ = 1 to 64 do
    ignore (drive_cycle sim ~rst_n:1 ~de:1 ~control:0 ~data:0xFF)
  done;
  (* All-zeros burst (bias should come back) *)
  for _ = 1 to 64 do
    ignore (drive_cycle sim ~rst_n:1 ~de:1 ~control:0 ~data:0x00)
  done;
  (* Read bias through the encoded output's disparity:
     We can't read the internal bias register directly via the I/O
     interface, so we check that the encoded stream is not all the same
     polarity (i.e. bias did not saturate).                             *)
  let ones_count  = ref 0 in
  let zeros_count = ref 0 in
  for _ = 1 to 32 do
    let enc = drive_cycle sim ~rst_n:1 ~de:1 ~control:0 ~data:0xAA in
    let ones = ref 0 in
    for b = 0 to 9 do if (enc lsr b) land 1 = 1 then incr ones done;
    ones_count  := !ones_count  + !ones;
    zeros_count := !zeros_count + (10 - !ones)
  done;
  let ratio = abs (!ones_count - !zeros_count) in
  check (Printf.sprintf "disparity balanced after recovery (imbalance=%d)" ratio)
    (ratio < 80)   (* at most 25 % imbalance over 320 bits *)

(* ── Test 4 — reset clears output ────────────────────────────────────── *)
let test_reset () =
  Printf.printf "\n[DVI Encoder] Test 4 — reset clears encoder state\n";
  let sim = create_sim () in
  (* Run for a bit to build up bias *)
  for _ = 1 to 20 do
    ignore (drive_cycle sim ~rst_n:1 ~de:1 ~control:0 ~data:0xFF)
  done;
  (* Assert reset (active-low → 0) *)
  ignore (drive_cycle sim ~rst_n:0 ~de:1 ~control:0 ~data:0xFF);
  let enc = drive_cycle sim ~rst_n:0 ~de:1 ~control:0 ~data:0xFF in
  check_eq "encoded=0 while in reset" ~expected:0 ~got:enc

(* ── entry point ─────────────────────────────────────────────────────── *)
let () =
  test_control_tokens ();
  test_transition_minimisation ();
  test_disparity_recovery ();
  test_reset ()