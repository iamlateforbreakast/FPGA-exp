(* gowin_clkdiv.ml *)
open Hardcaml

module I = struct
  type 'a t =
    { hclkin : 'a
    ; resetn : 'a
    } 
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { clkout : 'a
    }
  [@@deriving hardcaml]
end

let create (_scope : Scope.t) (_i : _ I.t) = ()

let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
  let module H = Hierarchy.In_scope(I)(O) in
  H.hierarchical ~scope ~name:"gowin_clkdiv" ~instance:"inst1" create i