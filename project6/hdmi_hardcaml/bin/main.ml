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
module Tcard = Svo_tcard.Make(My_config)


module I = struct
  type 'a t =
    { clkin : 'a
    } 
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { tmds_clk_n : 'a
    ; tmds_clk_p : 'a
    }
  [@@deriving hardcaml]
end

module TopCircuit = Circuit.With_interface(I)(O)
let create (scope: Scope.t) (input: _ I.t)=
  let rpll = Gowin_rpll.hierarchical scope (Gowin_rpll.I.{clkin = input.clkin}) in
  let clkdiv = Gowin_clkdiv.hierarchical scope 
    (Gowin_clkdiv.I.{ hclkin = rpll.clkout; resetn = Signal.gnd }) in
  let tcard = Tcard.hierarchical scope (Tcard.I.{ clk = clkdiv.clkout; resetn = Signal.gnd }) in

  (* Instantiate the HDMI module with the required inputs *)
  let hdmi = Hdmi.hierarchical scope 
    (Hdmi.I.{ clk = Signal.gnd
            ; resetn = Signal.gnd
            ; out_axis_tready = Signal.gnd
            ; clk_pixel = clkdiv.clkout
            ; clk_5x_pixel = clkdiv.clkout
            ; locked = rpll.locked }) in

  {
    O.tmds_clk_n = Signal.gnd;
    O.tmds_clk_p = Signal.gnd;
  }
let circuit = 
  let scope = Scope.create ~flatten_design:false () in
  TopCircuit.create_exn ~name:"top" (create scope)

let output_mode = Rtl.Output_mode.To_file("hdmi.v")

let () = Rtl.output ~output_mode Verilog circuit