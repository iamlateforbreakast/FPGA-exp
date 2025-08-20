(* gowin_elvds_obuf.ml *)
open Hardcaml

module I = struct
  type 'a t =
    { inp : 'a [@name "I"]
    } 
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { out : 'a [@name "O"]
    ; out_b : 'a [@name "OB"]
    }
  [@@deriving hardcaml]
end

let create (_scope : Scope.t) (_i : _ I.t) =
  { O.out = Signal.gnd; O.out_b = Signal.gnd }

let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
  let module H = Hierarchy.In_scope(I)(O) in
  H.hierarchical ~scope ~name:"ELVDS_OBUF" ~instance:"inst1" create i
