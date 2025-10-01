(* top.ml *)
open Hardcaml

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
  end

  let create (scope: Scope.t) (input: _ I.t)=
    let open Signal in
    let open Always in
    let reg_sync_spec = Reg_spec.create ~clock:i.clock ~clear:i.i_reset () in
    let color_r = Variable.reg ~enable:vdd reg_sync_spec ~width:8 in
    let color_g = Variable.reg ~enable:vdd reg_sync_spec ~width:8 in
    let color_b = Variable.reg ~enable:vdd reg_sync_spec ~width:8 in
    let rpll = Gowin_rpll.hierarchical scope 
      (Gowin_rpll.I.{clkin = input.clock}) in
    let clkdiv = Gowin_clkdiv.hierarchical scope 
      (Gowin_clkdiv.I.{ hclkin = rpll.clkout
                      ; resetn = vdd
                      ; calib = gnd }) in
    let timing_gen = Vesa.hierarchical scope
      (Vesa.I.{ clock = clkdiv.clkout; i_resetn = vdd}) in
    (* Instantiate the HDMI module with the required inputs *)
    let dvi_tx = Dvi_tx.hierarchical scope 
      (Dvi_tx.I.{ i_serial_clk = Signal.gnd
                ; i_resetn = Signal.vdd
                ; i_rgb_clk = rpll.clkout
                ; i_rgb_vs = timing_gen.o_vsync
                ; i_rgb_hs = timing_gen.o_hsync
                ; i_rgb_de = timing_gen.o_data_en
                ; i_rgb_r = color_r
                ; i_rgb_g = color_g
                ; i_rgb_b = color_b }) in
              
    {
      O.tmds_clk_n = dvi_tx.tmds_clk_n;
      O.tmds_clk_p = dvi_tx.tmds_clk_p;
      O.tmds_d_n = dvi_tx.tmds_d_n;
      O.tmds_d_p = dvi_tx.tmds_d_p;
    }
end
