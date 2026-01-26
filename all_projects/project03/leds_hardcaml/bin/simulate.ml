(* simulate.ml *)

open Hardcaml
open Hardcaml_waveterm
open Project03_lib

module My_config = struct
  let clk_fre = 27
  
  let pattern = [ 0x01; 0x02; 0x04; 0x08; 0x10; 0x20] 
  let is_simulation = true
end

module MyLeds = Top.Make(My_config)

module Simulator = Cyclesim.With_interface (MyLeds.I)(MyLeds.O)

let testbench n =
  let scope = 
    Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let oc = open_out "leds.vcd" in
  let sim = 
    Simulator.create
      ~config:Cyclesim.Config.trace_all (MyLeds.create scope) |> Vcd.wrap oc in
  let inputs : _ MyLeds.I.t = Cyclesim.inputs sim in
  let _outputs : _ MyLeds.O.t = Cyclesim.outputs sim in
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
 
     
 
