(* top.ml *)
open Base
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

(*
    input             I_clk           , //27Mhz
    input             I_rst           ,
    input             I_key           ,
    output     [4:0]  O_led           ,
    output            running         ,
    output            O_tmds_clk_p    ,
    output            O_tmds_clk_n    ,
    output     [2:0]  O_tmds_data_p   ,//{r,g,b}
    output     [2:0]  O_tmds_data_n   
*)
  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; key : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { tmds_clk_p : 'a
      ; tmds_data_p : 'a[@bits 3]
	  ; leds : 'a[@bits 6]
      }
    [@@deriving hardcaml]
  end

  module MyPattern = Test_pattern.Make(X)
  module MyKey = Key_user_ctrl.Make(X)
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
        ~div_mode:"5" 
        ~hclkin:input.clock
        ~resetn:input.reset
        ~calib:gnd (* Tie CALIB to ground if unused *)
    in
  
    let dvi_encoder = MyDvi_encoder.hierarchical scope (
      MyDvi_encoder.I.{ pixel_clk = pixel_clk
                       ; reset = input.reset
                       })
    in
    (* Instanciate UART TX *)
    let dvi_tx = MyDvi_tx.hierarchical scope (
      MyDvi_tx.I.{ clock = input.clock
                  ; reset = input.reset
                  })
    in
    (* Instanciate leds *)
	  let leds = MyLeds.hierarchical scope (
	    MyLeds.I.{ reset=input.reset; clock=input.clock }) in

    { O.tmds_clk_p = zero 1; tmds_data_p = zero 3; O.leds = (~:(leds.leds)) }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
end
