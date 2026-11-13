open Hardcaml

module Sim_common = struct
  let clock_freq = 27_000_000
  let pll_freq = 81_000_000
  let is_simulation = true
  let pll_idiv_sel = 0
  let pll_fbdiv_sel = 2
  let pll_odiv_sel = 8
  let pll_primitive = `RPLL
  let gowin_device = "GW2A-18C"
  let normalize_reset x = x
end

module Sim_sdram = struct
  include Sim_common
  let normalize_led x = x
  let led_width = 6
end

module Sim_hyperram = struct
  include Sim_common
  let normalize_led x = x
  let led_width = 1
end

let bit r = Bits.to_int !r

(* ------------------------------------------------------------------------ *)
(* SDRAM behavioural model                                                  *)
(* ------------------------------------------------------------------------ *)

type sdram_model =
  { mem : (int, int) Hashtbl.t
  ; mutable bank : int
  ; mutable row : int
  ; mutable pending : (int * int) option
  ; mutable dq : int
  }

let sdram_model () =
  { mem = Hashtbl.create 8192
  ; bank = 0
  ; row = 0
  ; pending = None
  ; dq = 0
  }
;;

let sdram_word model ~addr =
  let col = (addr lsr 1) land 0xFF in
  (model.bank lsl 19) lor (model.row lsl 8) lor col
;;

let debug = Sys.getenv_opt "DEBUG" <> None

let sdram_step model ~clk ~prev_clk ~ras ~cas ~wen ~ba ~addr ~dq_out ~dq_oe =
  if clk = 1 && prev_clk = 0
  then (
    (* A read's data appears two CK clocks after the READ command.  The
       controller samples at phase 2 of the next CK cycle, so present it at
       the rising edge before that sample point. *)
    (match model.pending with
     | Some (word, n) when n <= 1 ->
       model.dq <- (match Hashtbl.find_opt model.mem word with Some v -> v | None -> 0);
       if debug then Printf.printf "  READ DATA word=%06x data=%08x\n" word model.dq;
       model.pending <- None
     | Some (word, n) -> model.pending <- Some (word, n - 1)
     | None -> ());
    if debug && not (ras = 1 && cas = 1 && wen = 1)
    then Printf.printf
           "sdram cmd ras=%d cas=%d wen=%d ba=%d addr=%03x row=%d bank=%d dq_oe=%d dq=%08x\n"
           ras cas wen ba addr model.row model.bank dq_oe dq_out;
    if ras = 0 && cas = 1 && wen = 1
    then (
      model.bank <- ba;
      model.row <- addr)
    else if ras = 1 && cas = 0 && wen = 0
    then (
      if dq_oe = 1
      then (
        let w = sdram_word model ~addr in
        if debug then Printf.printf "  WRITE word=%06x data=%08x\n" w dq_out;
        Hashtbl.replace model.mem w dq_out))
    else if ras = 1 && cas = 0 && wen = 1
    then (
      let w = sdram_word model ~addr in
      if debug then Printf.printf "  READ word=%06x\n" w;
      model.pending <- Some (w, 2)))
;;

let run_sdram () =
  let module Sys = Sdram_system.Make (Sim_sdram) in
  let module Simulator = Cyclesim.With_interface (Sys.I) (Sys.O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Sys.create scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let model = sdram_model () in
  inputs.reset := Bits.vdd;
  for _ = 1 to 8 do
    Cyclesim.cycle sim
  done;
  inputs.reset := Bits.gnd;
  let prev_clk = ref 0 in
  let result = ref `Timeout in
  let cycles = ref 0 in
  while !result = `Timeout && !cycles < 1_000_000 do
    let clk = bit outputs.sdram_clk in
    sdram_step
      model
      ~clk
      ~prev_clk:!prev_clk
      ~ras:(bit outputs.sdram_ras_n)
      ~cas:(bit outputs.sdram_cas_n)
      ~wen:(bit outputs.sdram_wen_n)
      ~ba:(Bits.to_int !(outputs.sdram_ba))
      ~addr:(Bits.to_int !(outputs.sdram_addr))
      ~dq_out:(Bits.to_int !(outputs.dq_out))
      ~dq_oe:(bit outputs.dq_oe);
    inputs.dq_in := Bits.of_int ~width:32 model.dq;
    Cyclesim.cycle sim;
    prev_clk := clk;
    incr cycles;
    if bit outputs.pass = 1 then result := `Pass;
    if bit outputs.fail = 1 then result := `Fail
  done;
  Printf.printf
    "SDRAM simulation: %s after %d cycles (leds=%d)\n"
    (match !result with `Pass -> "PASS" | `Fail -> "FAIL" | `Timeout -> "TIMEOUT")
    !cycles
    (Bits.to_int !(outputs.leds));
  !result
;;

(* ------------------------------------------------------------------------ *)
(* HyperRAM behavioural model                                               *)
(* ------------------------------------------------------------------------ *)

(* The model follows the HyperBus protocol rather than the controller's
   assumptions, so latency mistakes in the controller fail here:
   - RWDS is driven by the memory while CS# is low during the CA: high
     requests 2x initial latency.  The model picks 1x or 2x per transaction
     (pseudo-randomly), like a variable-latency device with refresh pending.
   - The CA is clocks 1..3.  Memory-space data starts at clock 3 + L*m
     (1-based, L = 3 initial latency clocks, m = 1 or 2): the latency count
     starts at CA clock 3.  This was measured on the Nano 4K with the debug
     trace (read data first appears in clock 6 at 1x).  Register (AS=1) writes have zero
     latency and are only acknowledged.
   - Data is a linear burst: every further CK clock with CS# low moves to the
     next word.  Writes honour the RWDS mask; a clock whose DQ or RWDS the
     controller is not driving writes a garbage word, so mis-timed write
     data corrupts neighbouring words.
   - Read data is driven edge-aligned with RWDS (high for the rising byte,
     low for the falling byte); RWDS stays low during read latency.
   - What the controller sees of DQ/RWDS is delayed by [hyper_pad_delay]
     fabric clocks, standing in for the pad round trip (CK out, tCKD, DQ
     in) measured with the debug trace on the Nano 4K. *)
let hyper_pad_delay = 1
let hyper_latency = 3
let garbage = 0xEE

type hyper_model =
  { mem : (int, int) Hashtbl.t
  ; rng : Random.State.t
  ; mutable prev_cs_n : int
  ; mutable ck_cycle : int  (** 1-based CK clock within the transaction *)
  ; mutable ca : int list  (** CA bytes, most recent first *)
  ; mutable addr : int
  ; mutable mult : int
  ; mutable kind : [ `Ca | `Config | `Read | `Write ]
  ; mutable write_hi : int
  ; mutable mask_hi : int
  ; mutable dq : int
  ; mutable rwds : int
  }

let hyper_model () =
  { mem = Hashtbl.create 8192
  ; rng = Random.State.make [| 13 |]
  ; prev_cs_n = 1
  ; ck_cycle = 0
  ; ca = []
  ; addr = 0
  ; mult = 1
  ; kind = `Ca
  ; write_hi = 0
  ; mask_hi = 0
  ; dq = 0
  ; rwds = 0
  }
;;

let hyper_data_start model = 3 + (hyper_latency * model.mult)

let hyper_decode_ca model =
  let ca = List.fold_left (fun acc b -> (acc lsl 8) lor b) 0 (List.rev model.ca) in
  let as_ = (ca lsr 46) land 1 in
  let rw = (ca lsr 47) land 1 in
  model.addr <- (((ca lsr 16) land 0x7FFFF) lsl 3) lor (ca land 0x7);
  model.kind <- (if as_ = 1 then `Config else if rw = 1 then `Read else `Write);
  (* After the CA the memory stops signalling latency.  For reads it holds
     RWDS low until data; for writes the controller owns RWDS. *)
  model.rwds <- 0;
  if debug
  then
    Printf.printf
      "hyper CA=%012Lx as=%d rw=%d addr=%06x latency=%dx\n"
      (Int64.of_int ca) as_ rw model.addr model.mult
;;

let hyper_step model ~ck ~prev_ck ~cs_n ~dq_out ~dq_oe ~rwds_out ~rwds_oe =
  let rising = ck = 1 && prev_ck = 0 in
  let falling = ck = 0 && prev_ck = 1 in
  if cs_n = 1
  then (
    model.dq <- 0;
    model.rwds <- 0)
  else (
    if model.prev_cs_n = 1
    then (
      model.ck_cycle <- 0;
      model.ca <- [];
      model.kind <- `Ca;
      model.mult <- (if Random.State.bool model.rng then 2 else 1);
      model.rwds <- (if model.mult = 2 then 1 else 0));
    let bus = if dq_oe = 1 then dq_out else garbage in
    let mask = if rwds_oe = 1 then rwds_out else 0 in
    let word_index () = model.ck_cycle - hyper_data_start model in
    if rising
    then (
      model.ck_cycle <- model.ck_cycle + 1;
      match model.kind with
      | `Ca -> model.ca <- bus :: model.ca
      | `Write when word_index () >= 0 ->
        model.write_hi <- bus;
        model.mask_hi <- mask
      | `Read when word_index () >= 0 ->
        let v =
          Option.value ~default:0 (Hashtbl.find_opt model.mem (model.addr + word_index ()))
        in
        model.dq <- (v lsr 8) land 0xFF;
        model.rwds <- 1
      | _ -> ());
    if falling
    then (
      match model.kind with
      | `Ca ->
        model.ca <- bus :: model.ca;
        if model.ck_cycle = 3 then hyper_decode_ca model
      | `Write when word_index () >= 0 ->
        let a = model.addr + word_index () in
        let old = Option.value ~default:0 (Hashtbl.find_opt model.mem a) in
        let hi = if model.mask_hi = 1 then (old lsr 8) land 0xFF else model.write_hi in
        let lo = if mask = 1 then old land 0xFF else bus in
        if debug then Printf.printf "hyper write %02x%02x addr=%06x\n" hi lo a;
        Hashtbl.replace model.mem a ((hi lsl 8) lor lo)
      | `Read when word_index () >= 0 ->
        let v =
          Option.value ~default:0 (Hashtbl.find_opt model.mem (model.addr + word_index ()))
        in
        if debug then Printf.printf "hyper read %04x addr=%06x\n" v (model.addr + word_index ());
        model.dq <- v land 0xFF;
        model.rwds <- 0
      | _ -> ()));
  model.prev_cs_n <- cs_n
;;

let run_hyperram () =
  let module Sys = Hyperram_system.Make (Sim_hyperram) in
  let module Simulator = Cyclesim.With_interface (Sys.I) (Sys.O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Simulator.create (Sys.create scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let model = hyper_model () in
  inputs.reset := Bits.vdd;
  for _ = 1 to 8 do
    Cyclesim.cycle sim
  done;
  inputs.reset := Bits.gnd;
  let pad = Queue.create () in
  for _ = 1 to hyper_pad_delay do
    Queue.push (0, 0) pad
  done;
  let prev_ck = ref 0 in
  let result = ref `Timeout in
  let cycles = ref 0 in
  while !result = `Timeout && !cycles < 1_000_000 do
    let ck = bit outputs.hpram_ck in
    hyper_step
      model
      ~ck
      ~prev_ck:!prev_ck
      ~cs_n:(bit outputs.hpram_cs_n)
      ~dq_out:(Bits.to_int !(outputs.dq_out))
      ~dq_oe:(bit outputs.dq_oe)
      ~rwds_out:(bit outputs.rwds_out)
      ~rwds_oe:(bit outputs.rwds_oe);
    Queue.push (model.dq, model.rwds) pad;
    let dq, rwds = Queue.pop pad in
    inputs.dq_in := Bits.of_int ~width:8 dq;
    inputs.rwds_in := Bits.of_int ~width:1 rwds;
    Cyclesim.cycle sim;
    prev_ck := ck;
    incr cycles;
    if bit outputs.done_ = 1 && debug
    then Printf.printf "hyper done dout=%04x\n" (Bits.to_int !(outputs.dout));
    if bit outputs.pass = 1 then result := `Pass;
    if bit outputs.fail = 1
    then (
      if debug then Printf.printf "hyper FAIL dout=%04x\n" (Bits.to_int !(outputs.dout));
      result := `Fail)
  done;
  Printf.printf
    "HyperRAM simulation: %s after %d cycles (led=%d)\n"
    (match !result with `Pass -> "PASS" | `Fail -> "FAIL" | `Timeout -> "TIMEOUT")
    !cycles
    (Bits.to_int !(outputs.led));
  !result
;;
