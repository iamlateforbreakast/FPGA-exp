(* gowin_rpll.ml *)
open Hardcaml

module I = struct
  type 'a t =
    { clkin : 'a
    } 
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { lock : 'a
    ; clkout : 'a
    }
  [@@deriving hardcaml]
end

let create (_scope : Scope.t) (_i : _ I.t) =
  { O.clkout = Signal.gnd; O.lock = Signal.gnd }

let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
  let module H = Hierarchy.In_scope(I)(O) in
  H.hierarchical ~scope ~name:"gowin_rpll" ~instance:"inst1" create i
