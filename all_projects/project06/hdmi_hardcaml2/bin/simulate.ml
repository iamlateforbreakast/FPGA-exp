(* simulate.ml *)

open Hardcaml
open Project06_lib

module My_config = struct
  let svo_mode = "640x480"
end

module MyVesa = Vesa.Make(My_config)

module Simulator = Cyclesim.With_interface(MyScreen.I)(MyVesa.O)

let testbench n =
  let scope = 
    Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let oc = open_out "vesa.vcd" in
  let sim = 
    Simulator.create
      ~config:Cyclesim.Config.trace_all (MyScreen.create scope) |> Vcd.wrap oc in
  let inputs : _ MyScreen.I.t = Cyclesim.inputs sim in
  let _outputs : _ MyScreen.O.t = Cyclesim.outputs sim in
  let waves, sim = Waveform.create sim in

  inputs.i_reset := Bits.vdd;

  for _i = 0 to n do
    Cyclesim.cycle sim
  done;
  close_out oc;
  waves

let () =
  let waves = testbench 1000 in
  Hardcaml_waveterm_interactive.run ~wave_width:5 ~signals_width:30 waves
