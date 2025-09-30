(* top.ml *)
open hardcaml

module type Config = Config.S

module Make (X : Config) = struct
  
  module I = struct
    type 'a t =
      { clock :    'a [@bits 1]  (* Need to be called clock for simulation *)
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { o_tmds_clk_p :  'a [@bits 1]
      ; o_tmds_clk_n :  'a [@bits 1]
      ; o_tmds_data_p : 'a [@bits 3]
      ; o_tmds_data_n : 'a [@bits 3]
      }
    [@@deriving hardcaml]
  end

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

  let create (scope: Scope.t) (input: _ I.t)=
    let open module Signal in
    let rpll = Gowin_rpll.hierarchical scope 
      (Gowin_rpll.I.{clkin = input.clock}) in
    let clkdiv = Gowin_clkdiv.hierarchical scope 
      (Gowin_clkdiv.I.{ hclkin = rpll.clkout
                    ; resetn = rpll.lock 
                    ; calib = Signal.vdd }) in
    (* Instantiate the HDMI module with the required inputs *)
    let dvi_tx = Dvi_tx.hierarchical scope 
      (Dvi_tx.I.{ clk = Signal.gnd
            ; resetn = Signal.vdd
            ; out_axis_tready = Signal.gnd
            ; clk_pixel = clkdiv.clkout
            ; clk_5x_pixel = rpll.clkout
            ; locked = rpll.lock }) in
    {
      O.tmds_clk_n = dvi_tx.tmds_clk_n;
      O.tmds_clk_p = dvi_tx.tmds_clk_p;
      O.tmds_d_n = dvi_tx.tmds_d_n;
      O.tmds_d_p = dvi_tx.tmds_d_p;
    }
end
