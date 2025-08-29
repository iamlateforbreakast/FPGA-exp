(* simulation.ml *)

open Hardcaml
open Hardcaml_waveterm
open Project6_lib

module My_config = struct
  let svo_mode = "640x480V"
  let svo_frame_rate = 60
  let svo_bits_per_pixel = 24
  let svo_bits_per_red = 8
  let svo_bits_per_green = 8
  let svo_bits_per_blue = 8
  let svo_bits_per_alpha = 0
  let svo_hor_pixels = 640
end

module Hdmi = Svo_hdmi.Make(My_config)

module Simulator = Cyclesim.With_interface (Hdmi.I)(Hdmi.O)

let testbench n =
  let scope = 
    Scope.create 
      ~auto_label_hierarchical_ports:true
      ~flatten_design:true () in
  let sim = 
    Simulator.create
      ~config:Cyclesim.Config.trace_all (Hdmi.create scope) in
      let waves, sim = Waveform.create sim in

  for _i = 0 to n do
    Cyclesim.cycle sim
  done;
  waves

let () =
  let waves = testbench 15 in
  Hardcaml_waveterm_interactive.run ~wave_width:5 ~signals_width:30 waves

     
