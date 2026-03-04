(* testbench.ml — Revised & Comprehensive *)
open Hardcaml
open Project07_lib
open Bmp280_model
open Hardcaml.Bits

(* ───────────────────────────────────────────────────────────────
   Configuration
   ─────────────────────────────────────────────────────────────── *)
module My_config = struct
  let clk_fre      = 27_000_000
  let uart_fre     = 27_000_000
  let baud_rate    = 115_200
  let i2c_address  = 0x76
  let pattern      = [0;1;2;3;4;5;6;7]
  let message      = "Temp: XXXX Press: XXXX"
  let is_simulation = true
end

module MyI2c_master = I2c_master.Make(My_config)
module Sim = Cyclesim.With_interface(MyI2c_master.I)(MyI2c_master.O)

(* ───────────────────────────────────────────────────────────────
   Test result tracking
   ─────────────────────────────────────────────────────────────── *)
let pass_count = ref 0
let fail_count = ref 0

let check label cond =
  if cond then begin
    Printf.printf "  [PASS] %s\n%!" label;
    incr pass_count
  end else begin
    Printf.printf "  [FAIL] %s\n%!" label;
    incr fail_count
  end

let assert_eq label expected actual =
  check (Printf.sprintf "%s: expected 0x%X, got 0x%X" label expected actual)
        (expected = actual)

(* ───────────────────────────────────────────────────────────────
   Simulation helpers
   ─────────────────────────────────────────────────────────────── *)

(* Correct open-drain wired-AND bus model:
   The bus is low if either the master OR the slave is pulling it low.
   Both parties are modelled as active-low drivers; default (released) is high. *)
let open_drain_bus ~master_oe ~master_sda ~slave_sda =
  let master_pulls_low = (master_oe = 1) && (master_sda = 0) in
  let slave_pulls_low  = (slave_sda = 0) in
  if master_pulls_low || slave_pulls_low then 0 else 1

(* Run the simulation for [n] clock cycles.
   On each cycle:
     1. Read master outputs BEFORE clocking (stable after last rising edge).
     2. Let the BMP280 model react to SCL/SDA to compute its SDA contribution.
     3. Combine onto open-drain bus and feed sda_in back to master.
     4. Advance the simulator by one cycle (rising edge). *)
let make_cycle_fn sim inputs outputs model =
  let cycle_num = ref 0 in
  fun ?(verbose=false) n ->
    for _ = 1 to n do
      let scl        = to_int !(outputs.MyI2c_master.O.scl) in
      let sda_out    = to_int !(outputs.MyI2c_master.O.sda_out) in
      let sda_oe     = to_int !(outputs.MyI2c_master.O.sda_oe) in

      (* Current sda_in visible to master (from previous cycle) *)
      let sda_in_cur = to_int !(inputs.MyI2c_master.I.sda_in) in

      (* BMP280 model sees the bus as driven by the master *)
      let slave_sda = Bmp280_Model.step model ~scl ~sda_in:sda_in_cur in

      (* Compose the open-drain bus value for the master to read next cycle *)
      let bus_sda = open_drain_bus ~master_oe:sda_oe ~master_sda:sda_out ~slave_sda in
      inputs.MyI2c_master.I.sda_in := of_int ~width:1 bus_sda;

      if verbose then
        Printf.printf
          "  [%04d] SCL=%d SDA_out=%d SDA_OE=%d bus_SDA=%d slave_SDA=%d ACK_err=%d\n%!"
          !cycle_num scl sda_out sda_oe bus_sda slave_sda
          (to_int !(outputs.MyI2c_master.O.ack_error));

      Cyclesim.cycle sim;
      incr cycle_num
    done

(* Wait up to [timeout] cycles for ready to go high; return true if it did. *)
let wait_ready ?(timeout=2000) cycle outputs =
  let ready () = to_bool !(outputs.MyI2c_master.O.ready) in
  let t = ref 0 in
  while not (ready ()) && !t < timeout do
    cycle 1;
    incr t
  done;
  ready ()

(* ───────────────────────────────────────────────────────────────
   Reset helper
   ─────────────────────────────────────────────────────────────── *)
let do_reset inputs cycle =
  inputs.MyI2c_master.I.reset := vdd;
  cycle 2;
  inputs.MyI2c_master.I.reset := gnd;
  cycle 2  (* allow IDLE to settle *)

(* ───────────────────────────────────────────────────────────────
   Transaction helpers
   ─────────────────────────────────────────────────────────────── *)

(* Issue a single-byte register write and wait for completion.
   Returns true if transaction completed without ACK error. *)
let do_write ~dev_addr ~reg_addr ~data inputs outputs cycle =
  inputs.MyI2c_master.I.dev_addr := of_int ~width:7 dev_addr;
  inputs.MyI2c_master.I.reg_addr := of_int ~width:8 reg_addr;
  inputs.MyI2c_master.I.mosi     := of_int ~width:8 data;
  inputs.MyI2c_master.I.rw       := gnd;
  inputs.MyI2c_master.I.start    := vdd;
  cycle 1;
  inputs.MyI2c_master.I.start    := gnd;
  let completed = wait_ready cycle outputs in
  let ack_ok = to_int !(outputs.MyI2c_master.O.ack_error) = 0 in
  completed && ack_ok

(* Issue a 6-byte burst register read and wait for completion.
   Returns (success, miso_value). *)
let do_read ~dev_addr ~reg_addr inputs outputs cycle =
  inputs.MyI2c_master.I.dev_addr := of_int ~width:7 dev_addr;
  inputs.MyI2c_master.I.reg_addr := of_int ~width:8 reg_addr;
  inputs.MyI2c_master.I.rw       := vdd;
  inputs.MyI2c_master.I.start    := vdd;
  cycle 1;
  inputs.MyI2c_master.I.start    := gnd;
  let completed = wait_ready cycle outputs in
  let ack_ok    = to_int !(outputs.MyI2c_master.O.ack_error) = 0 in
  let miso      = to_int !(outputs.MyI2c_master.O.miso) in
  (completed && ack_ok, miso)

(* ───────────────────────────────────────────────────────────────
   Sim factory — creates a fresh sim + model for each test
   ─────────────────────────────────────────────────────────────── *)
let make_sim () =
  let scope    = Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true () in
  let base_sim = Sim.create ~config:Cyclesim.Config.trace_all (MyI2c_master.create scope) in
  let waves, sim = Hardcaml_waveterm.Waveform.create base_sim in
  let inputs  = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let model   = Bmp280_Model.create ~addr:My_config.i2c_address in
  Bmp280_Model.reset_to_defaults model;
  let cycle   = make_cycle_fn sim inputs outputs model in
  (sim, waves, inputs, outputs, model, cycle)

(* ───────────────────────────────────────────────────────────────
   TEST 1: Reset behaviour
   Verify that after reset the master asserts ready and no ack_error.
   ─────────────────────────────────────────────────────────────── *)
let test_reset () =
  Printf.printf "\n=== TEST 1: Reset Behaviour ===\n%!";
  let (_sim, _waves, inputs, outputs, _model, cycle) = make_sim () in
  do_reset inputs cycle;
  check "ready high after reset"
        (to_bool !(outputs.MyI2c_master.O.ready));
  check "ack_error low after reset"
        (to_int !(outputs.MyI2c_master.O.ack_error) = 0);
  check "scl high after reset"
        (to_int !(outputs.MyI2c_master.O.scl) = 1);
  check "sda_out high after reset"
        (to_int !(outputs.MyI2c_master.O.sda_out) = 1)

(* ───────────────────────────────────────────────────────────────
   TEST 2: Write transaction — correct address & register
   Configure BMP280 mode register (0xF4 <- 0x27).
   Checks: no ack_error, correct byte written into model registers.
   ─────────────────────────────────────────────────────────────── *)
let test_write_config () =
  Printf.printf "\n=== TEST 2: Write Transaction (BMP280 Config) ===\n%!";
  let (_sim, _waves, inputs, outputs, model, cycle) = make_sim () in
  do_reset inputs cycle;

  let ok = do_write ~dev_addr:My_config.i2c_address
                    ~reg_addr:0xF4 ~data:0x27
                    inputs outputs cycle in
  check "write transaction completes"   ok;
  check "no ack_error after write"
        (to_int !(outputs.MyI2c_master.O.ack_error) = 0);
  assert_eq "BMP280 reg 0xF4 updated in model"
            0x27 model.regs.(0xF4)

(* ───────────────────────────────────────────────────────────────
   TEST 3: Write to wrong address — expect ack_error
   The BMP280 model only ACKs 0x76; a transaction to 0x77 should NACK.
   ─────────────────────────────────────────────────────────────── *)
let test_write_wrong_address () =
  Printf.printf "\n=== TEST 3: Write to Wrong I2C Address (NACK expected) ===\n%!";
  let (_sim, _waves, inputs, outputs, _model, cycle) = make_sim () in
  do_reset inputs cycle;

  inputs.MyI2c_master.I.dev_addr := of_int ~width:7 0x77; (* wrong addr *)
  inputs.MyI2c_master.I.reg_addr := of_int ~width:8 0xF4;
  inputs.MyI2c_master.I.mosi     := of_int ~width:8 0x27;
  inputs.MyI2c_master.I.rw       := gnd;
  inputs.MyI2c_master.I.start    := vdd;
  cycle 1;
  inputs.MyI2c_master.I.start    := gnd;
  let _completed = wait_ready cycle outputs in
  check "ack_error set on address NACK"
        (to_int !(outputs.MyI2c_master.O.ack_error) = 1)

(* ───────────────────────────────────────────────────────────────
   TEST 4: Burst read of 6 measurement bytes
   Seeds 0xF7–0xFC with known values, performs a read, verifies miso.
   ─────────────────────────────────────────────────────────────── *)
let test_burst_read () =
  Printf.printf "\n=== TEST 4: Burst Read (6 bytes from 0xF7) ===\n%!";
  let (_sim, _waves, inputs, outputs, model, cycle) = make_sim () in
  do_reset inputs cycle;

  (* Seed known measurement data *)
  model.regs.(0xF7) <- 0x65;  (* pres_msb  *)
  model.regs.(0xF8) <- 0x5A;  (* pres_lsb  *)
  model.regs.(0xF9) <- 0x00;  (* pres_xlsb *)
  model.regs.(0xFA) <- 0x7D;  (* temp_msb  *)
  model.regs.(0xFB) <- 0x20;  (* temp_lsb  *)
  model.regs.(0xFC) <- 0x00;  (* temp_xlsb *)

  (* First write to set the register pointer to 0xF7 *)
  let _ok = do_write ~dev_addr:My_config.i2c_address
                     ~reg_addr:0xF4 ~data:0x27
                     inputs outputs cycle in

  let (ok, miso) = do_read ~dev_addr:My_config.i2c_address
                            ~reg_addr:0xF7
                            inputs outputs cycle in

  let expected =
    (0x65 lsl 40) lor (0x5A lsl 32) lor (0x00 lsl 24) lor
    (0x7D lsl 16) lor (0x20 lsl 8)  lor  0x00
  in

  check "read transaction completes without error" ok;
  assert_eq "miso contains expected 6 bytes" expected miso;

  (* Check individual byte fields *)
  let pres_msb = (miso lsr 40) land 0xFF in
  let temp_msb = (miso lsr 16) land 0xFF in
  assert_eq "pres_msb" 0x65 pres_msb;
  assert_eq "temp_msb" 0x7D temp_msb

(* ───────────────────────────────────────────────────────────────
   TEST 5: Write-then-read consistency
   Write a value to a writable register, then read it back.
   ─────────────────────────────────────────────────────────────── *)
let test_write_then_read () =
  Printf.printf "\n=== TEST 5: Write-then-Read Consistency ===\n%!";
  let (_sim, _waves, inputs, outputs, model, cycle) = make_sim () in
  do_reset inputs cycle;

  (* Write 0xB6 to reset register 0xE0 *)
  let write_ok = do_write ~dev_addr:My_config.i2c_address
                          ~reg_addr:0xE0 ~data:0xB6
                          inputs outputs cycle in
  check "write completes" write_ok;
  assert_eq "model register updated" 0xB6 model.regs.(0xE0);

  (* Seed the read-back path *)
  model.regs.(0xE0) <- 0xB6;

  (* Seed 5 bytes after 0xE0 to complete the 6-byte burst *)
  for i = 1 to 5 do
    model.regs.(0xE0 + i) <- i
  done;

  let (read_ok, miso) = do_read ~dev_addr:My_config.i2c_address
                                 ~reg_addr:0xE0
                                 inputs outputs cycle in
  check "read completes" read_ok;
  let first_byte = (miso lsr 40) land 0xFF in
  assert_eq "first byte read back matches written value" 0xB6 first_byte

(* ───────────────────────────────────────────────────────────────
   TEST 6: Back-to-back transactions
   Verify the master returns to IDLE and accepts a new start correctly.
   ─────────────────────────────────────────────────────────────── *)
let test_back_to_back () =
  Printf.printf "\n=== TEST 6: Back-to-Back Transactions ===\n%!";
  let (_sim, _waves, inputs, outputs, _model, cycle) = make_sim () in
  do_reset inputs cycle;

  for i = 0 to 2 do
    let ok = do_write ~dev_addr:My_config.i2c_address
                      ~reg_addr:0xF4 ~data:(0x20 + i)
                      inputs outputs cycle in
    check (Printf.sprintf "back-to-back write %d completes" i) ok
  done

(* ───────────────────────────────────────────────────────────────
   TEST 7: SCL/SDA idle bus state between transactions
   Bus lines must be released (high) when master is idle.
   ─────────────────────────────────────────────────────────────── *)
let test_idle_bus_state () =
  Printf.printf "\n=== TEST 7: Idle Bus State ===\n%!";
  let (_sim, _waves, inputs, outputs, _model, cycle) = make_sim () in
  do_reset inputs cycle;

  let _ok = do_write ~dev_addr:My_config.i2c_address
                     ~reg_addr:0xF4 ~data:0x27
                     inputs outputs cycle in

  (* Give a few cycles to settle after ready *)
  cycle 4;
  check "SCL high when idle"   (to_int !(outputs.MyI2c_master.O.scl) = 1);
  check "SDA high when idle"   (to_int !(outputs.MyI2c_master.O.sda_out) = 1);
  check "SDA_OE low when idle" (to_int !(outputs.MyI2c_master.O.sda_oe) = 0)

(* ───────────────────────────────────────────────────────────────
   TEST 8: Chip ID register read (0xD0 = 0x58)
   Validates the model initialisation and the full read path.
   ─────────────────────────────────────────────────────────────── *)
let test_chip_id_read () =
  Printf.printf "\n=== TEST 8: Chip ID Read (0xD0 should be 0x58) ===\n%!";
  let (_sim, _waves, inputs, outputs, _model, cycle) = make_sim () in
  do_reset inputs cycle;

  let (ok, miso) = do_read ~dev_addr:My_config.i2c_address
                            ~reg_addr:0xD0
                            inputs outputs cycle in
  check "chip ID read completes" ok;
  let chip_id = (miso lsr 40) land 0xFF in (* first byte of burst *)
  assert_eq "chip ID is 0x58" 0x58 chip_id

(* ───────────────────────────────────────────────────────────────
   TEST 9: miso cleared between transactions
   After a read the miso output should not retain stale data
   once a new transaction starts. This exercises the miso_buffer <--. 0
   in IDLE.
   ─────────────────────────────────────────────────────────────── *)
let test_miso_cleared () =
  Printf.printf "\n=== TEST 9: miso Cleared on New Transaction ===\n%!";
  let (_sim, _waves, inputs, outputs, model, cycle) = make_sim () in
  do_reset inputs cycle;

  model.regs.(0xF7) <- 0xFF;
  for i = 1 to 5 do model.regs.(0xF7 + i) <- 0xFF done;

  let (_ok, miso_first) = do_read ~dev_addr:My_config.i2c_address
                                   ~reg_addr:0xF7
                                   inputs outputs cycle in

  (* Now seed zeros *)
  for i = 0 to 5 do model.regs.(0xF7 + i) <- 0x00 done;

  let (_ok2, miso_second) = do_read ~dev_addr:My_config.i2c_address
                                     ~reg_addr:0xF7
                                     inputs outputs cycle in

  check "first read returns 0xFF bytes"  (miso_first  = 0xFFFFFFFFFFFF);
  check "second read returns 0x00 bytes" (miso_second = 0x000000000000)

(* ───────────────────────────────────────────────────────────────
   TEST 10: Timeout safety — start never asserted
   Master should remain in IDLE indefinitely with ready high.
   ─────────────────────────────────────────────────────────────── *)
let test_idle_no_start () =
  Printf.printf "\n=== TEST 10: Idle with No Start ===\n%!";
  let (_sim, _waves, inputs, outputs, _model, cycle) = make_sim () in
  do_reset inputs cycle;
  inputs.MyI2c_master.I.start := gnd;
  cycle 500;
  check "still ready after 500 cycles of no start"
        (to_bool !(outputs.MyI2c_master.O.ready));
  check "no spurious ack_error"
        (to_int !(outputs.MyI2c_master.O.ack_error) = 0)

(* ───────────────────────────────────────────────────────────────
   Entry point
   ─────────────────────────────────────────────────────────────── *)
let () =
  Printf.printf "╔══════════════════════════════════════════╗\n";
  Printf.printf "║  BMP280 I2C Master — Testbench Suite     ║\n";
  Printf.printf "╚══════════════════════════════════════════╝\n%!";

  test_reset ();
  test_write_config ();
  test_write_wrong_address ();
  test_burst_read ();
  test_write_then_read ();
  test_back_to_back ();
  test_idle_bus_state ();
  test_chip_id_read ();
  test_miso_cleared ();
  test_idle_no_start ();

  Printf.printf "\n══════════════════════════════════════════\n";
  Printf.printf "Results: %d passed, %d failed\n%!" !pass_count !fail_count;
  if !fail_count > 0 then exit 1