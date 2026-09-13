(* test_encoder_reference.ml
   Cross-checks Dvi_encoder.Make against a bit-exact port of the reference
   Verilog (hdmi_verilog/dvi_encoder.v) implemented in plain OCaml integer
   arithmetic.  This catches disparity-tracking bugs that a hand-picked
   fixed test vector (e.g. feeding 0xAA) cannot: any divergence between the
   Hardcaml bias register and this reference model, over an arbitrary data
   stream, shows up as a mismatched `encoded` output on the very next cycle
   the running disparity affects the encoding.
   ──────────────────────────────────────────────────────────────────────
   Regression coverage for two bugs found by inspection in
   lib/dvi_encoder.ml:
     1. `disparity` was computed as `{1'b0, ones_in_qm} - 8` (via
        [concat_lsb]) instead of `{ones_in_qm, 1'b0} - 8` (via
        [concat_msb]) — i.e. `ones_in_qm - 8` instead of
        `2*ones_in_qm - 8`.
     2. The non-invert, non-forced branch's bias update was missing the
        Verilog's `~` on `qm[8]`: `bias - {~qm[8],1'b0} + disparity`.
*)

open Hardcaml
open Project06_lib
open Test_utils

module Enc = Dvi_encoder.Make (Test_config.Res_480p)

(* ── Bit-exact reference model of hdmi_verilog/dvi_encoder.v ─────────── *)

let mask n v = v land ((1 lsl n) - 1)

let ones_of ~width v =
  let c = ref 0 in
  for b = 0 to width - 1 do
    if (v lsr b) land 1 = 1 then incr c
  done;
  !c

(* qm_0/qm_1 as in the Verilog: qm[i] is data[0] xor(/xnor) data[1..i] *)
let compute_qm data =
  let bit i = (data lsr i) land 1 in
  let qm_0 =
    let acc = ref (bit 0) in
    let bits = Array.make 8 (bit 0) in
    bits.(0) <- bit 0;
    for i = 1 to 7 do
      acc := !acc lxor bit i;
      bits.(i) <- !acc
    done;
    let v = ref 0 in
    for i = 0 to 7 do
      v := !v lor (bits.(i) lsl i)
    done;
    (1 lsl 8) lor !v (* qm_0[8] = 1 *)
  in
  let qm_1 =
    let acc = ref (bit 0) in
    let bits = Array.make 8 (bit 0) in
    bits.(0) <- bit 0;
    for i = 1 to 7 do
      acc := !acc lxor (1 - bit i);
      bits.(i) <- !acc
    done;
    let v = ref 0 in
    for i = 0 to 7 do
      v := !v lor (bits.(i) lsl i)
    done;
    !v (* qm_1[8] = 0 *)
  in
  let ones_in_data = ones_of ~width:8 data in
  if ones_in_data > 4 || (ones_in_data = 4 && bit 0 = 0) then qm_1 else qm_0

type state = { mutable bias : int (* 5-bit, wraps mod 32 *) }

let control_token = function
  | 0 -> 0b1101010100
  | 1 -> 0b0010101011
  | 2 -> 0b0101010100
  | 3 -> 0b1010101011
  | _ -> invalid_arg "control_token"

(* One clock's worth of reference-model computation: given the previous
   [bias] and this cycle's (de, control, data), returns (encoded, new_bias). *)
let reference_step (s : state) ~de ~control ~data =
  if not de then begin
    s.bias <- 0;
    control_token control
  end
  else begin
    let qm = compute_qm data in
    let qm_7_0 = mask 8 qm in
    let qm_8 = (qm lsr 8) land 1 in
    let ones_in_qm = ones_of ~width:8 qm_7_0 in
    let disparity = mask 5 ((ones_in_qm lsl 1) - 8) in
    (* subtraction below is done mod 32 throughout, matching a 5-bit reg *)
    let sub a b = mask 5 (a - b) in
    let add a b = mask 5 (a + b) in
    if s.bias = 0 || ones_in_qm = 4 then begin
      let encoded =
        ((1 - qm_8) lsl 9) lor (qm_8 lsl 8)
        lor (if qm_8 = 1 then qm_7_0 else mask 8 (lnot qm_7_0))
      in
      s.bias <- (if qm_8 = 1 then add s.bias disparity else sub s.bias disparity);
      encoded
    end
    else begin
      let invert = ((s.bias lsr 4) land 1) lxor (if ones_in_qm > 4 then 1 else 0) in
      if invert = 1 then begin
        let encoded = (1 lsl 9) lor (qm_8 lsl 8) lor (mask 8 (qm_7_0 lxor 0xFF)) in
        s.bias <- sub (add s.bias (qm_8 lsl 1)) disparity;
        encoded
      end
      else begin
        let encoded = (0 lsl 9) lor (qm_8 lsl 8) lor qm_7_0 in
        s.bias <- add (sub s.bias ((1 - qm_8) lsl 1)) disparity;
        encoded
      end
    end
  end

(* ── Drive Hardcaml sim and reference model in lockstep ──────────────── *)

let create_sim () =
  let module Sim = Cyclesim.With_interface (Enc.I) (Enc.O) in
  Sim.create (Enc.create (Scope.create ~flatten_design:true ()))

let hw_cycle sim ~rst_n ~de ~control ~data =
  let i : Bits.t ref Enc.I.t = Cyclesim.inputs sim in
  i.rst_n := Bits.of_int ~width:1 rst_n;
  i.de := Bits.of_int ~width:1 (if de then 1 else 0);
  i.control := Bits.of_int ~width:2 control;
  i.data := Bits.of_int ~width:8 data;
  Cyclesim.cycle sim;
  let o : Bits.t ref Enc.O.t = Cyclesim.outputs sim in
  port_int o.encoded

(* A stream that exercises all three encoded-output branches: forced
   (bias=0 initially), the "invert" branch and the "no-invert" branch,
   across a long pseudo-random run. *)
let test_matches_reference () =
  Printf.printf
    "\n[DVI Encoder] Reference-model cross-check (bit-exact vs. Verilog algorithm)\n";
  let sim = create_sim () in
  let ref_state = { bias = 0 } in
  (* Release reset on both models. *)
  ignore (hw_cycle sim ~rst_n:0 ~de:false ~control:0 ~data:0);
  let seed = 42 in
  Random.init seed;
  let n_cycles = 5000 in
  let first_mismatch = ref None in
  for cycle = 1 to n_cycles do
    let de = Random.int 10 <> 0 (* mostly active video, occasional blanking *) in
    let control = Random.int 4 in
    let data = Random.int 256 in
    let hw = hw_cycle sim ~rst_n:1 ~de ~control ~data in
    let reference = reference_step ref_state ~de ~control ~data in
    if hw <> reference && !first_mismatch = None then
      first_mismatch := Some (cycle, hw, reference)
  done;
  match !first_mismatch with
  | None -> pass (Printf.sprintf "encoder matches reference model over %d cycles" n_cycles)
  | Some (cycle, hw, reference) ->
    fail
      "encoder matches reference model"
      (Printf.sprintf "diverged at cycle %d: hw=%d (0x%03x) reference=%d (0x%03x)"
         cycle hw hw reference reference)

let () = test_matches_reference ()
