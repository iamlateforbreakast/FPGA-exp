(* simulate.ml *)

open Hardcaml
open Hardcaml_waveterm
open Project04_lib

module My_config = struct
  let clk_fre = 27
  let cycle_delay = 10
  let ws2812_num = 0
  let ws2812_width = 6
  let colors = [ 0xFF0000; 0x00FF00; 0x0000FF ]  (* Red, Green, Blue *)
  let is_simulation = true
end

module MyWs2812 = Top.Make(My_config)

module Simulator = Cyclesim.With_interface (MyWs2812.I)(MyWs2812.O)

let testbench n =
  let scope = 
    Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let oc = open_out "ws2812.vcd" in
  let sim = 
    Simulator.create
      ~config:Cyclesim.Config.trace_all (MyWs2812.create scope) |> Vcd.wrap oc in
  let inputs : _ MyWs2812.I.t = Cyclesim.inputs sim in
  let _outputs : _ MyWs2812.O.t = Cyclesim.outputs sim in
  let waves, sim = Waveform.create sim in

  inputs.reset := Bits.gnd;

  for _i = 0 to n do
    Cyclesim.cycle sim
  done;
  close_out oc;
  waves

let () =
  let waves = testbench 1000 in
  Hardcaml_waveterm_interactive.run ~wave_width:5 ~signals_width:30 waves
 
     
 
