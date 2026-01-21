(* simulation.ml *)

open Hardcaml
open Hardcaml_waveterm
open Project08_lib

module My_config = struct
  let file_name = "image.hex"
  let startup_wait = 10
  let clk_div = 4 (* SPI clock = 27MHz / 4 *)
  let commands = [ 0xAE; 0x80; 0XAF]
  let is_simulation = true
end

module MyScreen = Screen.Make(My_config)

module Simulator = Cyclesim.With_interface (MyScreen.I)(MyScreen.O)

let testbench n =
  let scope = 
    Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let oc = open_out "screen.vcd" in
  let sim = 
    Simulator.create
      ~config:Cyclesim.Config.trace_all (MyScreen.create scope) |> Vcd.wrap oc in
  let inputs : _ MyScreen.I.t = Cyclesim.inputs sim in
  let _outputs : _ MyScreen.O.t = Cyclesim.outputs sim in
  let waves, sim = Waveform.create sim in

  inputs.i_reset := Bits.gnd;

  for _i = 0 to n do
    Cyclesim.cycle sim
  done;
  close_out oc;
  waves

let () =
  let waves = testbench 1000 in
  Hardcaml_waveterm_interactive.run ~wave_width:5 ~signals_width:30 waves
 
     
 
