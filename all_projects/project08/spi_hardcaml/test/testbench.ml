(* simulation.ml *)

open Hardcaml
open Hardcaml_waveterm
open Project8_lib

module My_config = struct
  let file_name = "image.hex"
  let startup_wait = 10
  let clk_div = 4 (* SPI clock = 27MHz / 4 *)
  let commands = [ 0xAE; 0x80; 0XAF]
end

module MyScreen = Screen.Make(My_config)

module Simulator = Cyclesim.With_interface(I)(O)

let testbench () =
  let scope = Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let sim = Simulator.create create in
  let inputs : _ MyScreen.I.t = Cyclesim.inputs sim in
  let outputs : _ MyScreen.O.t = Cyclesim.outputs sim in
  let step =
    (* inputs.clear := if clear=1 then Bits.vdd else Bits.gnd;
    inputs.incr := if incr=1 then Bits.vdd else Bits.gnd; *)
    Printf.printf "io_sclk=%i io_sdin=%i io_cs=%i io_dc=%i io_reset=%i\n"
      outputs.io_sclk outputs.io_stdin outputs.io_cs outputs.io_dc outputs.io_reset;
    Cyclesim.cycle sim
  
  in
  step;
  step;
  step;
  step;
  step;
  step;
;;
let%expect_test "counter" =
  testbench ();
  [%expect {|
    clear=0 incr=0 dout=0
    clear=0 incr=1 dout=0
    clear=0 incr=1 dout=1
    clear=1 incr=0 dout=2
    clear=0 incr=0 dout=0
    clear=0 incr=0 dout=0
  |}]
;;
