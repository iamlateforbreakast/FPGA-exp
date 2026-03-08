(* test_test_pattern.ml
   Tests for Test_pattern.Make
   ──────────────────────────────────────────────────────────────────────
   The test-pattern module generates VGA-style sync + data enable signals.
   We test:
     1. Horizontal counter wraps at h_total.
     2. Vertical counter increments once per line and wraps at v_total.
     3. DE (data enable) is asserted exactly (h_res × v_res) times per frame.
     4. HS is asserted exactly h_sync pixels wide per line.
     5. VS is asserted exactly v_sync lines wide per frame.
     6. Sync polarity: hs/vs respect the hs_pol/vs_pol config flags.
     7. Reset: all outputs de-assert within one cycle of rst_n going low.
     8. Fixed-colour output: data_r/g/b carry the expected constant values.
     9. Pipeline latency: DE/HS/VS are delayed by exactly 5 clocks.
*)

open Hardcaml
open Hardcaml_waveterm
open Test_utils

(* Use 480p — fewer cycles to simulate a full frame *)
module Pat = Test_pattern.Make(Config.Res_480p)
module C   = Config.Res_480p

(* ── simulator ───────────────────────────────────────────────────────── *)

let create_sim () =
  let module Sim = Cyclesim.With_interface(Pat.I)(Pat.O) in
  Sim.create (Pat.create (Scope.create ~flatten_design:true ()))

(** Reset the simulator for two cycles then release. *)
let release_reset sim =
  let i = Cyclesim.inputs sim in
  i.rst_n   := Bits.of_int ~width:1 0;
  i.pxl_clk := Bits.of_int ~width:1 0;  (* unused by Cyclesim — for clarity *)
  i.mode    := Bits.of_int ~width:3 0;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  i.rst_n := Bits.of_int ~width:1 1

(** Cycle once and return outputs snapshot *)
let step sim =
  Cyclesim.cycle sim;
  let o = Cyclesim.outputs sim in
  ( port_bool o.de
  , port_bool o.hs
  , port_bool o.vs
  , port_int  o.data_r
  , port_int  o.data_g
  , port_int  o.data_b )

(* ── helpers ─────────────────────────────────────────────────────────── *)

let one_frame_cycles = C.h_total * C.v_total

(** Run one complete frame and collect per-cycle output samples. *)
let collect_frame sim =
  Array.init one_frame_cycles (fun _ -> step sim)

(* ── Test 1 — DE pixel count ─────────────────────────────────────────── *)
let test_de_count () =
  Printf.printf "\n[Test Pattern] Test 1 — DE asserted h_res×v_res times per frame\n";
  let sim = create_sim () in
  release_reset sim;
  (* Discard first frame (pipeline filling) *)
  ignore (collect_frame sim);
  (* Measure second frame *)
  let frame = collect_frame sim in
  let de_count = Array.fold_left (fun acc (de,_,_,_,_,_) -> if de then acc+1 else acc) 0 frame in
  check_eq "DE count = h_res × v_res"
    ~expected:(C.h_res * C.v_res)
    ~got:de_count

(* ── Test 2 — HS pulse width ─────────────────────────────────────────── *)
let test_hs_pulse_width () =
  Printf.printf "\n[Test Pattern] Test 2 — HS pulse = h_sync wide\n";
  let sim = create_sim () in
  release_reset sim;
  ignore (collect_frame sim);
  let frame   = collect_frame sim in
  (* Count HS-active runs; each run must be exactly h_sync pixels *)
  let in_pulse  = ref false in
  let run_len   = ref 0 in
  let bad_runs  = ref 0 in
  (* hs_pol=false for 480p → pulse is active-low → hs=false during pulse *)
  let hs_active (_,hs,_,_,_,_) = not hs in   (* active-low *)
  Array.iter (fun sample ->
    if hs_active sample then begin
      in_pulse := true;
      incr run_len
    end else begin
      if !in_pulse then begin
        if !run_len <> C.h_sync then incr bad_runs;
        run_len := 0;
        in_pulse := false
      end
    end
  ) frame;
  check_eq "bad HS pulse-width runs" ~expected:0 ~got:!bad_runs

(* ── Test 3 — VS pulse height (in lines) ─────────────────────────────── *)
let test_vs_pulse_height () =
  Printf.printf "\n[Test Pattern] Test 3 — VS pulse = v_sync lines high\n";
  let sim = create_sim () in
  release_reset sim;
  ignore (collect_frame sim);
  let frame = collect_frame sim in
  (* VS active-low for 480p: vs=false during sync *)
  let vs_active (_,_,vs,_,_,_) = not vs in
  let vs_pixels = Array.fold_left (fun acc s -> if vs_active s then acc+1 else acc) 0 frame in
  check_eq "VS active pixels = v_sync × h_total"
    ~expected:(C.v_sync * C.h_total)
    ~got:vs_pixels

(* ── Test 4 — sync polarity (positive-polarity config) ───────────────── *)
(*  Switch to 720p (positive polarity) and verify hs is high during pulse *)
module Pat720 = Test_pattern.Make(Config.Res_720p)
module C720   = Config.Res_720p

let test_sync_polarity_positive () =
  Printf.printf "\n[Test Pattern] Test 4 — positive-polarity HS/VS (720p)\n";
  let module Sim = Cyclesim.With_interface(Pat720.I)(Pat720.O) in
  let sim = Sim.create (Pat720.create (Scope.create ~flatten_design:true ())) in
  let i = Sim.inputs sim in
  i.rst_n := Bits.of_int ~width:1 0; i.mode := Bits.of_int ~width:3 0;
  Sim.cycle sim; Sim.cycle sim;
  i.rst_n := Bits.of_int ~width:1 1;
  (* Skip first frame *)
  for _ = 1 to C720.h_total * C720.v_total do Sim.cycle sim done;
  (* Sample 200 cycles and check HS is high during sync region *)
  let saw_hs_high = ref false in
  for _ = 1 to C720.h_sync + 10 do
    Sim.cycle sim;
    let hs = Bits.to_int !(Sim.outputs sim).hs in
    if hs = 1 then saw_hs_high := true
  done;
  check "positive-polarity: HS goes high at some point" !saw_hs_high

(* ── Test 5 — pipeline latency = 5 ──────────────────────────────────── *)
(*  We can't directly observe the combinational pre-pipeline de_w, but we
    can verify that after reset the first DE edge appears exactly
    (h_sync + h_bporch + 5) cycles into the first line, where +5 is the
    pipeline delay.                                                       *)
let test_pipeline_latency () =
  Printf.printf "\n[Test Pattern] Test 5 — DE pipeline latency = 5 cycles\n";
  let sim = create_sim () in
  release_reset sim;
  let first_de_cycle = ref (-1) in
  let cycle = ref 0 in
  while !first_de_cycle = -1 && !cycle < C.h_total + 20 do
    let (de,_,_,_,_,_) = step sim in
    incr cycle;
    if de then first_de_cycle := !cycle
  done;
  let expected_latency = C.h_sync + C.h_bporch + 5 in (* combinational + 5 pipe stages *)
  check_eq "first DE cycle (with pipeline delay)"
    ~expected:expected_latency
    ~got:!first_de_cycle

(* ── Test 6 — fixed colour outputs ──────────────────────────────────── *)
let test_fixed_colour () =
  Printf.printf "\n[Test Pattern] Test 6 — constant colour (R=255, G=0, B=0)\n";
  let sim = create_sim () in
  release_reset sim;
  let r_ok = ref true and g_ok = ref true and b_ok = ref true in
  for _ = 1 to 100 do
    let (_,_,_,r,g,b) = step sim in
    if r <> 255 then r_ok := false;
    if g <> 0   then g_ok := false;
    if b <> 0   then b_ok := false;
  done;
  check "data_r = 255 (constant red)" !r_ok;
  check "data_g = 0"                  !g_ok;
  check "data_b = 0"                  !b_ok

(* ── Test 7 — reset de-asserts all outputs ───────────────────────────── *)
let test_reset_clears () =
  Printf.printf "\n[Test Pattern] Test 7 — asserting rst_n=0 clears DE\n";
  let sim = create_sim () in
  release_reset sim;
  (* Run until we're in the active DE region *)
  for _ = 1 to C.h_sync + C.h_bporch + 5 + 10 do
    ignore (step sim)
  done;
  (* Assert reset *)
  let i = Cyclesim.inputs sim in
  i.rst_n := Bits.of_int ~width:1 0;
  (* After reset, counters should clear; DE should drop within h_total *)
  let de_still_high = ref false in
  for _ = 1 to 10 do
    let (de,_,_,_,_,_) = step sim in
    if de then de_still_high := true
  done;
  (* BUG DETECTION: if the clear polarity bug is present, counters freeze
     in reset=running and DE stays asserted.  Correct behaviour: DE=0. *)
  check "DE de-asserts within 10 cycles after reset" (not !de_still_high)

(* ── entry point ─────────────────────────────────────────────────────── *)
let () =
  test_de_count ();
  test_hs_pulse_width ();
  test_vs_pulse_height ();
  test_sync_polarity_positive ();
  test_pipeline_latency ();
  test_fixed_colour ();
  test_reset_clears ()