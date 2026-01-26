(* top.ml *)
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { leds : 'a[@bits 6]
      }
    [@@deriving hardcaml]
  end

  module MyLeds = Leds.Make(X)
	
  let create (scope : Scope.t) (input : Signal.t I.t) : Signal.t O.t =
    let leds = MyLeds.hierarchical scope (
	     MyLeds.I.{ reset=input.reset; clock=input.clock }) in
    let _ = Signal.(leds.leds -- "dbg_leds") in
    (* Return circuit output value *)
    { O.leds = (~:(leds.leds)) }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"top_level" ~instance:"inst1" create i
    
end
