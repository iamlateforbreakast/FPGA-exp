(* ws2812.ml *)
open Hardcaml
open Signal

module type Config = Config.S

module Make (X : Config) = struct
  
  module I = struct
  type 'a t = {
    clk   : 'a;
    reset : 'a;
    color : 'a; [@bits 24]
  } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      data : 'a;
    } [@@deriving hardcaml]
  end

  let create (_scope: Scope.t) (_i: Signal.t I.t) : Signal.t O.t =
    {O.data = gnd}

  let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"ws2812" ~instance:"inst1" create i
end
