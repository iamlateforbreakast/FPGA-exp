(* top.ml *)
open Base
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clock : 'a
      ; I_rst : 'a
      ; I_key : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { O_tmds_clk_p : 'a
      ; O_tmds_data_p : 'a[@bits 3]
	    ; O_led : 'a[@bits 6]
      }
    [@@deriving hardcaml]
  end

  module MyPattern = Test_pattern.Make(X)
  module MyKey = Key_ctrl.Make(X)
  module MyDvi_tx = Dvi_tx.Make(X)
  module MyDvi_encoder = Dvi_encoder.Make(X)
  module MyLeds = Leds.Make(X)

  let clkdiv ~div_mode ~hclkin ~resetn ~calib =
    let _ = X.clk_fre in
    let parameters = 
      List.map
      ~f:(fun (name, value) -> Parameter.create ~name ~value)
        [
          "DIV_MODE", Parameter.Value.Int div_mode;  (* Clock Divider *)
           "GSREN", Parameter.Value.Bool false;  (* Global Set/Reset Enable *)
        ] in
      Instantiation.create
        ()
        ~name:"CLKDIV" (* Must match the Gowin primitive name *)
        ~parameters:parameters
        ~inputs:[
          "HCLKIN", hclkin;
          "RESETN", resetn;
          "CALIB",  calib;
        ]
        ~outputs:[ "CLKOUT", 1 ]
      |> fun outputs -> Map.find_exn outputs "CLKOUT"
  
  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =

    (* Instanciate the CLKDIV primitive *)
	  let pixel_clk = clkdiv 
        ~div_mode:5 
        ~hclkin:input.clock
        ~resetn:input.I_rst
        ~calib:gnd (* Tie CALIB to ground if unused *)
    in
  
    (* Instanciate Test Pattern*)
    let test_pattern = MyPattern.hierarchical scope (
      MyPattern.I.{ rst_n = ~:(input.I_rst)
                  ; pxl_clk = pixel_clk
                  ; mode = zero 3 })
    in
    let dvi_tx = MyDvi_tx.hierarchical scope (
      MyDvi_tx.I.{ serial_clk = input.clock
                  ; rst_n = ~:(input.I_rst)
                  ; rgb_clk = pixel_clk
                  ; rgb_vs = test_pattern.vs
                  ; rgb_hs = test_pattern.hs
                  ; rgb_de = test_pattern.de
                  ; rgb_r = test_pattern.data_r
                  ; rgb_g = test_pattern.data_g
                  ; rgb_b = test_pattern.data_b
                  })
    in
    (* Instanciate leds *)
	  let leds = MyLeds.hierarchical scope (
	    MyLeds.I.{ reset=input.I_rst; clock=input.clock }) in

    { O.O_tmds_clk_p = dvi_tx.tmds_clk_p; O.O_tmds_data_p = dvi_tx.tmds_data_p; O.O_led = (~:(leds.leds)) }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
end
