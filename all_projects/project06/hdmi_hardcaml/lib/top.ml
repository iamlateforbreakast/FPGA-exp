(* top.ml *)
open Base
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clock : 'a [@rtlname "I_clk"]
      ; rst : 'a [@rtlname "I_rst"]
      ; key : 'a [@rtlname "I_key"]
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { tmds_clk_p : 'a [@rtlname "O_tmds_clk_p"]
      ; tmds_data_p : 'a [@rtlname "O_tmds_data_p"] [@bits 3]
	    ; led : 'a [@rtlname "O_led"] [@bits 6]
      }
    [@@deriving hardcaml]
  end

  module MyPattern = Test_pattern.Make(X)
  module MyKey = Key_ctrl.Make(X)
  module MyDvi_tx = Dvi_tx.Make(X)
  module MyDvi_encoder = Dvi_encoder.Make(X)
  module MyLeds = Leds.Make(X)

  let clkdiv ~div_mode ~hclkin ~resetn ~calib =
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

  let rpll ~inst ~clkin =
    let parameters = 
      List.map
      ~f:(fun (name, value) -> Parameter.create ~name ~value)
        [
          "FCLKIN", Parameter.Value.String "27";          (* Input clock frequency *)
          "IDIV_SEL", Parameter.Value.Int 0;               (* Input divider *)
          "FBDIV_SEL", Parameter.Value.Int 10;             (* Feedback divider *)
          "ODIV_SEL", Parameter.Value.Int 8;               (* Output divider for CLKOUT *)
          "DYN_SDIV_SEL", Parameter.Value.Int 2;           (* Static divider for CLKOUTD (pclk) *)
          "DEVICE", Parameter.Value.String "GW2A-18C";     (* Your specific chip *)
        ] in
    let m = Instantiation.create
      ~name:"rPLL"
      ~instance:inst
      ~parameters:parameters
      ~inputs:[
        "CLKIN", clkin;
        "CLKFB", Signal.gnd;
        "RESET", Signal.gnd;
        "RESET_P", Signal.gnd;
      ]
      ~outputs:[
        "CLKOUT", 1;  (* Serial Clock (fclk) *)
        "CLKOUTD", 1; (* Pixel Clock (pclk) - usually fclk / 5 *)
        "LOCK", 1;
      ]
      ()
    in
    (Map.find_exn m "CLKOUT", Map.find_exn m "CLKOUTD", Map.find_exn m "LOCK")
  
  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    (* Instanciate the rPLL primitive *)
	  let (fclk, _pclk_from_pll, _pll_lock) = rpll
	    ~inst:"pll"
	    ~clkin:input.clock
	  in
    (* Instanciate the CLKDIV primitive *)
	  let pixel_clk = clkdiv 
        ~div_mode:5 
        ~hclkin:fclk
        ~resetn:input.rst
        ~calib:gnd (* Tie CALIB to ground if unused *)
    in
  
    (* Instanciate Test Pattern*)
    let test_pattern = MyPattern.hierarchical scope (
      MyPattern.I.{ rst_n = ~:(input.rst)
                  ; pxl_clk = pixel_clk
                  ; mode = zero 3 })
    in
    let dvi_tx = MyDvi_tx.hierarchical scope (
      MyDvi_tx.I.{ serial_clk = input.clock
                  ; rst_n = ~:(input.rst)
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
	    MyLeds.I.{ reset=input.rst; clock=input.clock }) in

    { O.tmds_clk_p = dvi_tx.tmds_clk_p; O.tmds_data_p = dvi_tx.tmds_data_p; O.led = (~:(leds.leds)) }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
end
