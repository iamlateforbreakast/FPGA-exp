(* main.ml *)
open Hardcaml
open Project6_lib

module My_config = struct
  let svo_mode = 640
  let svo_frame_rate = 60
  let svo_bits_per_pixel = 24
  let svo_bits_per_red = 8
  let svo_bits_per_green = 8
  let svo_bits_per_blue = 8
  let svo_bits_per_alpha = 0
end

module Hdmi = Svo_hdmi.Make(My_config)

module SimpleCircuit = Circuit.With_interface(Hdmi.I)(Hdmi.O)

let circuit = 
  let scope = Scope.create ~flatten_design:false () in
  let rpll = SimpleCircuit.create_exn ~name:"gowin_rpll" (create scope) in
  let clkdiv = SimpleCircuit.create_exn ~name:"gowin_clkdiv" (create scope) in
  SimpleCircuit.create_exn ~name:"svo_hdmi" (Hdmi.create scope)

let output_mode = Rtl.Output_mode.To_file("hdmi.v")

let () = Rtl.output ~output_mode Verilog circuit