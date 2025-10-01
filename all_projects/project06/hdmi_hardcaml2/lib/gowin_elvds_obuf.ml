(* gowin_elvds_obuf.ml *)
open Hardcaml

module I = struct
  type 'a t =
    { i : 'a [@rtlname "I"]
    } 
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { o : 'a [@rtlname "O"]
    ; ob : 'a [@rtlname "OB"]
    }
  [@@deriving hardcaml]
end

let create (_scope : Scope.t) (i : _ I.t) =
  let module Inst = Instantiation.With_interface(I)(O) in
  Inst.create ~name:"ELVDS_OBUF" ~instance:"inst" i

let hierarchical (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
  let module H = Hierarchy.In_scope(I)(O) in
  H.hierarchical ~scope ~name:"gowin_elvds_obuf" ~instance:"inst" create i
