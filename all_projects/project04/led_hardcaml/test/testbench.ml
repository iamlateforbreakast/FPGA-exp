(* testbench.ml *)

open Hardcaml
open Project04_lib

module My_config = struct
  let clk_fre = 27_000_000
  let cycle_delay = 27_000_000
  let ws2812_num = 0
  let ws2812_width = 6
  let colors = [ 0xFF0000; 0x00FF00; 0x0000FF ]  (* Red, Green, Blue *)
end

module MyWs2812 = Ws2812.Make(My_config)

module Simulator = Cyclesim.With_interface(MyWs2812.I)(MyWs2812.O)


let testbench () =
  let scope = Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true () in
  let sim = Simulator.create (MyWs2812.create scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let step cycle =
    Printf.printf "cycle=%d data=%d\n" cycle (Bits.to_int !(outputs.data))
  in

  (* Initialize inputs *)
  inputs.color := Bits.of_int ~width:24 0xFF0000;

  (* Reset module *)
  inputs.reset := Bits.vdd;
  Cyclesim.cycle sim;
  step 0;
  inputs.reset := Bits.gnd;

  for _cycle_reset = 1 to ((My_config.clk_fre/1000) - 1) do
    Cyclesim.cycle sim;
  done;
  step ((My_config.clk_fre/1000) - 1);

  for byte = 0 to 2 do
    for bit = 0 to 7 do
      for _cycle = 0 to 6 do
        Cyclesim.cycle sim;
      done;
      step (6 + bit * 32 + byte * 8 * 32 + (My_config.clk_fre/1000));
      for _cycle = 7 to 19 do
        Cyclesim.cycle sim;
      done;
      step (19 + bit * 32 + byte * 8 * 32 + (My_config.clk_fre/1000));
      for _cycle = 20 to 31 do
        Cyclesim.cycle sim;
      done;
      step (31 + bit * 32 + byte * 8 * 32 + (My_config.clk_fre/1000));
    done;
  done;
  () 


let%expect_test "ws2812" =
  testbench ();
  [%expect {|
    cycle=1 data=0
    cycle=2 data=0
    cycle=3 data=0
    cycle=4 data=0
    cycle=5 data=0
    cycle=6 data=0
    cycle=7 data=0
    cycle=8 data=0 
    cycle=9 data=0
    cycle=10 data=0
    cycle=11 data=0
    cycle=12 data=0
    cycle=13 data=0
    cycle=14 data=0
    cycle=15 data=0
    cycle=16 data=0
    cycle=17 data=0
    cycle=18 data=0
    cycle=19 data=0
    cycle=20 data=0
    cycle=21 data=0
    cycle=22 data=0
    cycle=23 data=0
    cycle=24 data=0
    cycle=25 data=0
    cycle=26 data=0
  |}]
