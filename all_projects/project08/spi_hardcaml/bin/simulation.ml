(* simulation.ml *)

open Hardcaml
open Hardcaml_waveterm
open Project8_lib

module My_config = struct
  let file_name = "image.hex"
  let startup_wait = 2
end

module MyScreen = Screen.Make(My_config)

module Simulator = Cyclesim.With_interface (MyScreen.I)(MyScreen.O)

let testbench n =
  let scope = 
    Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let sim = 
    Simulator.create
      ~config:Cyclesim.Config.trace_all (MyScreen.create scope) in
  let inputs : _ MyScreen.I.t = Cyclesim.inputs sim in
  let _outputs : _ MyScreen.O.t = Cyclesim.outputs sim in
  let waves, sim = Waveform.create sim in

  inputs.i_reset := Bits.vdd;
  for _i = 0 to n do
    Cyclesim.cycle sim
  done;
  waves

let () =
  let waves = testbench 15 in
  Hardcaml_waveterm_interactive.run ~wave_width:5 ~signals_width:30 waves

     
