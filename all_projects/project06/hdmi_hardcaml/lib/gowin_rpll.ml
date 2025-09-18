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

module Circuit = Circuit.With_interface(I)(O)

let parameters = 
  List.map
  (fun (name, value) -> Parameter.create ~name ~value)
  [
    "FBDIV", Parameter.Value.Int 5;  (* Feedback Divider *)
  ]

let create (_scope : Scope.t) (i : _ I.t) =
  let module Inst = Instantiation.With_interface(I)(O) in
  Inst.create ~name:"gowin_rpll" ~parameters i

let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
  let module H = Hierarchy.In_scope(I)(O) in
  H.hierarchical ~scope ~name:"gowin_rpll" ~instance:"inst1" create i
