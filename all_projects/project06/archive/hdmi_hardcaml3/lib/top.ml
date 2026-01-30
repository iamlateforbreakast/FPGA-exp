(* top.ml *)
open Hardcaml

module type Config = Config.S

module Make (X : Config) = struct
  
  module I = struct
    type 'a t =
      { clock :    'a [@bits 1]  (* input sys_clk; *)
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { o_tmds_clk_p :  'a [@bits 1] (* output O_tmds_clk_p; *)
      ; o_tmds_clk_n :  'a [@bits 1] (* output O_tmds_clk_n; *)
      ; o_tmds_data_p : 'a [@bits 3] (* output [2:0] O_tmds_data_p; *)
      ; o_tmds_data_n : 'a [@bits 3] (* output [2:0] O_tmds_data_n; *)
      ; o_led : 'a [@bits 1]         (* output O_led; *)
      }
    [@@deriving hardcaml]
  end

  module MyVesa = Vesa.Make(X)
  module MyDvi_tx = Dvi_tx.Make(X)
  
  let create (scope: Scope.t) (i: _ I.t)=
    let open Signal in
    let open Always in
    let reg_sync_spec = Reg_spec.create ~clock:i.clock ~clear:gnd () in
    (* reg [2:0] colors = 0; *)
    let color_r = Variable.reg ~enable:vdd reg_sync_spec ~width:8 in
    let color_g = Variable.reg ~enable:vdd reg_sync_spec ~width:8 in
    let color_b = Variable.reg ~enable:vdd reg_sync_spec ~width:8 in
    (* gowin_rpll pll(dvi_clk, sys_clk); *)
    let rpll = Gowin_rpll.hierarchical scope 
      (Gowin_rpll.I.{clkin = i.clock}) in
    (* CLKDIV div_by_5(.RESETN(1'b1), .HCLKIN(dvi_clk), .CLKOUT(pix_clk), .CALIB (1'b0)); *)
    (* defparam div_by_5.DIV_MODE="5"; *)
    (* defparam div_by_5.GSREN="false"; *)
    let clkdiv = Gowin_clkdiv.hierarchical scope 
      (Gowin_clkdiv.I.{ hclkin = rpll.clkout
                      ; resetn = vdd
                      ; calib = gnd }) in
    let timing_gen = MyVesa.hierarchical scope
      (MyVesa.I.{ clock = clkdiv.clkout; i_resetn = vdd}) in
    (* Instantiate the HDMI module with the required inputs *)
    let dvi_tx = MyDvi_tx.hierarchical scope 
      (MyDvi_tx.I.{ i_serial_clk = rpll.clkout
                  ; i_resetn = Signal.vdd
                  ; i_rgb_clk = clkdiv.clkout
                  ; i_rgb_vs = timing_gen.o_vsync
                  ; i_rgb_hs = timing_gen.o_hsync
                  ; i_rgb_de = timing_gen.o_data_en
                  ; i_rgb_r = color_r.value
                  ; i_rgb_g = color_g.value
                  ; i_rgb_b = color_b.value }) in
    let _ = Always.compile [
    color_r <--.  255;  (* Red = 255 *)
    color_g <--.  0;    (* Green = 0 *)
    color_b <--.  0;    (* Blue = 0 *)
    ] in
    {
      O.o_tmds_clk_n = dvi_tx.o_tmds_clk_n;
      O.o_tmds_clk_p = dvi_tx.o_tmds_clk_p;
      O.o_tmds_data_n = dvi_tx.o_tmds_d_n;
      O.o_tmds_data_p = dvi_tx.o_tmds_d_p;
      O.o_led = vdd;  (* Always on *)
    }
end
