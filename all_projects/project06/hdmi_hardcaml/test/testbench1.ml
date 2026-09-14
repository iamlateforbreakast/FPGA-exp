(* testbench1.ml *)
open Base
open Hardcaml
open Project06_lib
open Hardcaml_waveterm

module My_config = struct
  (* 1280x720 @ 60 Hz (CEA-861-D format 4). Pixel clock must match what
     lib/top.ml's rPLL (IDIV_SEL=3, FBDIV_SEL=54, ODIV_SEL=2) + CLKDIV
     (DIV_MODE=5) produce: 27 MHz * 55 / 4 / 5 = 74.25 MHz. *)
  let clk_fre = 74_250_000
  let h_total  = 1650
  let v_total  = 750
  let h_res    = 1280
  let v_res    = 720
  let h_sync   = 40
  let v_sync   = 5
  let h_bporch = 220
  let v_bporch = 20
  let hs_pol   = true
  let vs_pol   = true
  let pattern = [0;1;2;3;4;5;6;7]
  let is_simulation = false

  (* Unused by Dvi_encoder, required only to satisfy Config.S. *)
  let normalize_reset x = x
  let normalize_led x = x
  let led_width = 6
  let lvds_primitive = `TLVDS
  let pll_primitive = `RPLL
  let pll_idiv_sel = 2
  let pll_fbdiv_sel = 13
  let pll_odiv_sel = 4
  let gowin_device = "GW2A-18C"
end

module MyDviEncoder = Dvi_encoder.Make(My_config)
module Simulator = Cyclesim.With_interface (MyDviEncoder.I) (MyDviEncoder.O)

let testbench () =
  (* 1. Create the simulation *)
  let sim = Simulator.create (MyDviEncoder.create (Scope.create ())) in
  let inputs = Cyclesim.inputs sim in
  let _outputs = Cyclesim.outputs sim in

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
