(* top.ml *)
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
  module MyLeds = Leds.Make(X)

  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let sync_spec = Reg_spec.create ~clock:input.clock ~reset:input.reset () in

    (* Instanciate UART TX *)
    let dvi_tx = MyDvi_tx.hierarchical scope (
      MyDvi_tx.I.{ clock = input.clock
                  ; reset = input.reset
                  })
    in
    (* Instanciate leds *)
	  let leds = MyLeds.hierarchical scope (
	    MyLeds.I.{ reset=input.reset; clock=input.clock }) in

    { O.tmds_clk_p = zero 1; tmds_clk_p = zero 3; uart_tx.pin; O.leds = (~:(leds.leds)) }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
end
