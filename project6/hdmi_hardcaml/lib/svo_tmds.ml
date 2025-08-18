(* svo_tmds.ml *)
open Hardcaml

module type Config = Config.S

module Make (X : Config.S) = struct

  module I = struct
    type 'a t =
      { clk : 'a
      ; resetn : 'a
      ; de : 'a
      ; ctrl : 'a [@bits 2]
      ; din : 'a [@bits 8]
      } 
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { dout : 'a [@bits 10]
      }
    [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (_i : _ I.t) =
    { O.dout = Signal.gnd; }

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"svo_tmds" ~instance:"inst1" create i
end
